extern crate graphael;
extern crate hyper;
extern crate rustc_serialize;
extern crate url;
extern crate argparse;
#[macro_use]
extern crate log;


use graphael::{GraphDB};
use std::sync::Mutex;
use std::io;
use std::io::Write;
use std::fs;
use std::path;
use std::borrow::Cow;
use hyper::server::{Handler, Server, Request, Response};
use hyper::uri::RequestUri;
use hyper::status::StatusCode;
use hyper::header::ContentType;
use hyper::mime::{Mime, TopLevel, SubLevel, Attr, Value};
use rustc_serialize::json;
use url::{Url};
use std::net::{Ipv4Addr, SocketAddrV4};
use log::{LogRecord, LogLevel, LogMetadata, SetLoggerError, LogLevelFilter};
use argparse::{ArgumentParser, Store};

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
    graph: Mutex<GraphDB>,
    base_url: Url
}

impl GraphHandler {
    fn handle_static(&self, path: &str, res:Response) {
        let file_path = path::Path::new("_static").join(path.split_at(1).1);
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

    fn handle_query<'a, I: Iterator<Item=(Cow<'a, str>, Cow<'a, str>)>>(&self, mut res:Response, params: I) {
        {
            let headers = res.headers_mut();
            headers.set(
                ContentType(Mime(TopLevel::Application, SubLevel::Json,
                                 vec![(Attr::Charset, Value::Utf8)]))
            );
        }

        for (key, value) in params {
            if &key == "q" {
                match self.graph.lock() {
                    Ok(graph) => {
                        match  graph.match_paths(&*value) {
                            Ok(result) => {
                                let dag = result.to_dag();

                                let mut res = res.start().unwrap();

                                let encoded = json::encode(&dag).unwrap();
                                try_write!(res.write(b"{\"type\":\"success\",\"graph\":"));
                                try_write!(res.write(encoded.as_bytes()));
                                try_write!(res.write(b"}"));
                                try_write!(res.end());
                            },
                            Err(_) => {
                                self.handle_error(StatusCode::BadRequest, "Unable to parse path expression", true, res);
                            }
                        }
                    },
                    Err(_) => {

                    }
                }
                return
            }
        }

        self.handle_error(StatusCode::BadRequest, "Query parameter not set", true, res);



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
            if let Ok(url) =self.base_url.join(uri) {
                let path = url.path();
                info!("Accessed: {}", path);
                if path == "/query" {
                    self.handle_query(res, url.query_pairs())
                } else {
                    if path == "/" {
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
    let mut graph_file = String::new();
    let mut port = 6421;
    {
        let mut parser = ArgumentParser::new();
        parser.set_description("Web interface for queries graph database.");
        parser.refer(&mut graph_file).required()
            .add_argument("file", Store, "Database file to perform queries on");
        parser.refer(&mut port)
            .add_option(&["-p", "--port"], Store, "Port to listen on");
        parser.parse_args_or_exit();
    }
    info!("Opening graph db file \"{}\"",  &graph_file);
    let graph = match GraphDB::read_from_file(&graph_file) {
        Ok(graph) => graph,
        Err(e) => {
            writeln!(& mut std::io::stderr(), "Unable to open file \"{}\": {}", graph_file, e).unwrap();
            std::process::exit(1)
        }
    };
    let base_url = format!("http://localhost.com:{}", port);
    let listen_addr = SocketAddrV4::new(Ipv4Addr::new(0, 0, 0, 0), port);
    match Server::http(listen_addr) {
        Ok(server) => {
            info!("Listening at {}", base_url);
            server.handle(GraphHandler {
                graph: Mutex::new(graph),
                base_url: Url::parse(&base_url).unwrap()
            }).unwrap();
        },
        Err(e) => {
            writeln!(& mut std::io::stderr(), "Unable to start server on port {}: {}", port, e).unwrap();
        }
    }
}
