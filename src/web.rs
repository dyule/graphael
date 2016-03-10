extern crate graphael;
extern crate hyper;
extern crate rustc_serialize;
extern crate url;
#[macro_use]
extern crate log;


use graphael::{Graph};
use std::sync::Mutex;
use std::io;
use std::io::Write;
use std::fs;
use std::path;
use std::collections::HashMap;
use hyper::server::{Handler, Server, Request, Response};
use hyper::uri::RequestUri;
use hyper::status::StatusCode;
use hyper::header::ContentType;
use hyper::mime::{Mime, TopLevel, SubLevel, Attr, Value};
use rustc_serialize::json;
use url::{Url, UrlParser};
use graphael::matching::MatchingAutomaton;
use graphael::queries::parse_expression;
use log::{LogRecord, LogLevel, LogMetadata, SetLoggerError, LogLevelFilter};

struct SimpleLogger;

macro_rules! try_write {
    ($wr: expr) => (
        match $wr {
            Ok(_) => {},
            Err(e) => {error!("Sending error: {:?}", e)}
        }
    );
}

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &LogMetadata) -> bool {
        metadata.level() <= LogLevel::Info
    }

    fn log(&self, record: &LogRecord) {
        if self.enabled(record.metadata()) {
            println!("{} - {}", record.level(), record.args());
        }
    }
}

impl SimpleLogger {
    pub fn init() -> Result<(), SetLoggerError> {
    log::set_logger(|max_log_level| {
        max_log_level.set(LogLevelFilter::Info);
        Box::new(SimpleLogger)
    })
}
}

struct GraphHandler {
    graph: Mutex<Graph>,
    base_url: Url
}

impl GraphHandler {
    fn handle_static(&self, path: &str, res:Response) {
        let file_path = path::Path::new("_static").join(path.split_at(1).1);
        println!("{:?}", file_path);
        if let Ok(mut index_file) = fs::File::open(file_path) {
            let mut res = match res.start() {
                Ok(res) => res,
                Err(_) => return
            };
            try_write!(io::copy( &mut index_file, &mut res));
            try_write!(res.end());
        } else {
            self.handle_error(StatusCode::NotFound, "Page Not Found", false, res)
        }
    }

    fn handle_query(&self, mut res:Response, params: Option<Vec<(String, String)>>) {
        {
            let headers = res.headers_mut();
            headers.set(
                ContentType(Mime(TopLevel::Application, SubLevel::Json,
                                 vec![(Attr::Charset, Value::Utf8)]))
            );
        }
        match params {
            Some(params) => {
                let mut param_map = HashMap::new();
                for &(ref key, ref value) in params.iter() {
                    param_map.insert(key, value);
                }
                if let Some(query) = param_map.get(&"q".to_string()) {
                    //Write the graph as JSON

                    match  parse_expression(query) {
                        Ok(expression) => {
                            let automata = MatchingAutomaton::from_path_expression(expression);
                            match self.graph.lock() {
                                Ok(graph) => {
                                    let result = graph.match_paths(&automata);

                                    let dag = result.to_dag();

                                    let mut res = res.start().unwrap();

                                    let encoded = json::encode(&dag).unwrap();
                                    try_write!(res.write(b"{\"type\":\"success\",\"graph\":"));
                                    try_write!(res.write(encoded.as_bytes()));
                                    try_write!(res.write(b"}"));
                                    try_write!(res.end());
                                },
                                Err(_) => {
                                    // OH SHIT
                                }
                            }
                        },
                        Err(_) => {
                            self.handle_error(StatusCode::BadRequest, "Unable to parse path expression", true, res);
                        }
                    }
                } else {
                    self.handle_error(StatusCode::BadRequest, "Query parameter not set", true, res);
                }
            },
            None => {
                self.handle_error(StatusCode::BadRequest, "Query parameter not set", true, res);
            }
        }
    }

    fn handle_error(&self, status: StatusCode, message: &str, as_json:bool, mut res:Response) {
        {
            *res.status_mut() = status;
        }
        error!("{:?}", message);
        if as_json {
            let mut res = res.start().unwrap();
            try_write!(res.write(b"{\"type\":\"error\",\"message\":\""));
            try_write!(res.write(message.as_bytes()));
            try_write!(res.write(b"\"}"));
            try_write!(res.end());
        } else {
            try_write!(res.send(message.as_bytes()));
        }
    }
}

impl Handler for GraphHandler {
    fn handle(&self, req: Request, res: Response) {
        if let RequestUri::AbsolutePath(ref uri) = req.uri {
            if let Ok(url) = UrlParser::new().base_url(&self.base_url).parse(uri) {
                let path = url.serialize_path().unwrap();
                info!("Accessed: {}", path);
                if &path == "/query" {
                    self.handle_query(res, url.query_pairs())
                } else {
                    if &path == "/" {
                        self.handle_static("/index.html", res);
                    } else {
                        self.handle_static(&path, res);
                    }
                }
            }
        }
    }
}

fn main() {
    if let Err(_)  = SimpleLogger::init() {
        println!("Unable to initialize logging");
    }
    let graph = Graph::read_from_file("data/langs.graph".to_string());
    Server::http("0.0.0.0:6421").unwrap().handle(GraphHandler {
        graph: Mutex::new(graph),
        base_url: Url::parse("http://localhost.com:6421").unwrap()
    }).unwrap();
}
