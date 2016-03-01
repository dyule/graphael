extern crate graphael;
extern crate hyper;
extern crate rustc_serialize;
extern crate url;


use graphael::{Graph};
use std::sync::Mutex;
use std::io;
use std::io::Write;
use std::fs;
use std::collections::HashMap;
use hyper::server::{Handler, Server, Request, Response};
use hyper::uri::RequestUri;
use hyper::status::StatusCode;
use hyper::header::{Headers, ContentType};
use hyper::mime::{Mime, TopLevel, SubLevel, Attr, Value};
use rustc_serialize::json::{ToJson, self};
use url::{Url, UrlParser};
use graphael::matching::MatchingAutomaton;
use graphael::queries::parse_expression;

struct GraphHandler {
    graph: Mutex<Graph>,
    base_url: Url
}

impl GraphHandler {
    fn handle_index(&self, req: &Request, res:Response) {
        let mut index_file = fs::File::open("_static/index.html").unwrap();
        let mut res = match res.start() {
            Ok(res) => res,
            Err(_) => return
        };
        io::copy( &mut index_file, &mut res);
        res.end();
    }

    fn handle_query(&self, req: &Request, mut res:Response, params: Option<Vec<(String, String)>>) {
        match params {
            Some(params) => {
                let mut param_map = HashMap::new();
                for &(ref key, ref value) in params.iter() {
                    param_map.insert(key, value);
                }
                if let Some(query) = param_map.get(&"q".to_string()) {
                    //Write the graph as JSON
                    if let Ok(expression) = parse_expression(query) {
                        let automata = MatchingAutomaton::from_path_expression(expression);
                        match self.graph.lock() {
                            Ok(graph) => {
                                let result = graph.match_paths(&automata);

                                let dag = result.to_dag();
                                {
                                    let headers = res.headers_mut();
                                    headers.set(
                                        ContentType(Mime(TopLevel::Application, SubLevel::Json,
                                                         vec![(Attr::Charset, Value::Utf8)]))
                                    );
                                }
                                let mut res = res.start().unwrap();

                                let encoded = json::encode(&dag).unwrap();
                                res.write(encoded.as_bytes());
                                res.end();
                            },
                            Err(_) => {
                                // OH SHIT
                            }
                        }
                    }
                }
            },
            None => {}
        }
    }

    fn handle_error(&self, req: &Request, mut res:Response) {
        {
            *res.status_mut() = StatusCode::NotFound;
        }
        res.send(b"Unknown page");
    }
}

impl Handler for GraphHandler {
    fn handle(&self, req: Request, mut res: Response) {
        if let RequestUri::AbsolutePath(ref uri) = req.uri {
            if let Ok(url) = UrlParser::new().base_url(&self.base_url).parse(uri) {
                let path = url.serialize_path().unwrap();
                if &path == "/" {
                    self.handle_index(&req, res)
                } else if &path == "/query" {
                    self.handle_query(&req, res, url.query_pairs())
                } else {
                    self.handle_error(&req, res)
                }
            }
        }
    }
}

fn main() {
    let graph = Graph::read_from_file("data/langs.graph".to_string());
    Server::http("0.0.0.0:6421").unwrap().handle(GraphHandler {
        graph: Mutex::new(graph),
        base_url: Url::parse("http://localhost.com:6421").unwrap()
    }).unwrap();
}
