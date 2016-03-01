use nom::{multispace, IResult};
pub use queries::ast::{ASTNode, ASTEdge, ASTPath};
use std::str::{FromStr};
use ::PropVal;
use matching::NodeMatcher;

mod ast;

fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

fn is_alphanumeric(c: char) -> bool {
    c.is_alphanumeric()
}

// pub enum ASTNode<'a> {
//     NodeIDExpression(usize),
//     NodePropVal(&'a str, PropVal),
//     NodeProp(&'a str),
//     NodePropsList(Vec<ASTNode<'a>>),
// }

named!(node_expression<&str, NodeMatcher >, delimited!(
    tag_s!("("),
    chain!(
        opt!(multispace) ~
        node_exp: opt!(alt!(node_id_expression | node_props_expression)) ~
        opt!(multispace), || {
            match node_exp {
                Some(ref node) => node.clone(),
                None => NodeMatcher::Any
            }
        }
    ),
    tag_s!(")")
));

named!(node_id_expression<&str, NodeMatcher >, map_res!(
    take_while_s!(is_digit),
    |num| {
        match usize::from_str(num) {
            Ok(id) => Ok(NodeMatcher::Id(id)),
            Err(e) => Err(e)
    }
    }));

named!(node_props_expression<&str, NodeMatcher>, map!(delimited!(
    tag_s!("{"),
    chain!(
        opt!(multispace) ~
        prop_list: separated_list!(tag_s!(","), node_prop) ~
        opt!(multispace), || {
            prop_list.iter().fold(None, |acc, prop| {
                match acc {
                    Some(acc) => Some(NodeMatcher::And(Box::new(acc), Box::new(prop.clone()))),
                    None => Some(prop.clone())
                }
            })
        }),
    tag_s!("}")
), |props| {
    match props {
        Some(props) => props,
        None => NodeMatcher::Any
    }
}));

named!(node_prop<&str, NodeMatcher>, chain!(
    opt!(multispace) ~
    key: symbolic_name ~
    opt!(multispace) ~
    tag_s!(":") ~
    opt!(multispace) ~
    v: prop_val ~
    opt!(multispace), || {
        NodeMatcher::PropVal(key.to_string().into_boxed_str(), v)
    }
));

named!(symbolic_name<&str, &str>, alt!(
    escaped_symbolic_name |
    unescaped_symbolic_name
));
fn not_backtick(c: char) -> bool {
    c != '`'
}

fn not_quote(c: char) -> bool {
    c != '"'
}

named!(prop_val<&str, PropVal>, alt!(
    prop_val_int |
    prop_val_string
));

named!(escaped_symbolic_name<&str, &str>, chain!(
    tag_s!("`") ~
    name: take_while_s!(not_backtick) ~
    tag_s!("`"), || {
         name
    }
));

named!(unescaped_symbolic_name<&str, &str>, take_while_s!(is_alphanumeric));

named!(prop_val_int<&str, PropVal >, map_res!(
    take_while_s!(is_digit),
    |num| {
        match i64::from_str(num) {
            Ok(num) => Ok(PropVal::Int(num)),
            Err(e) => Err(e)
    }
    }));

named!(prop_val_string<&str, PropVal>, chain!(
    tag_s!("\"") ~
    v: take_while_s!(not_quote) ~
    tag_s!("\""), || {
         PropVal::String(v.to_string().into_boxed_str())
    }
));

named!(edge_expression<&str, ASTEdge>, chain!(
    tag_s!("-") ~
    opt!(multispace) ~
    labels: opt!(label_list) ~
    opt!(multispace) ~
    tag_s!(">"), || {
        match labels {
            Some(ref labels) => ASTEdge::LabelList(labels.clone()),
            None => ASTEdge::Any
        }
    }
));

named!(label_list<&str, Vec<&str> >, separated_list!(
    chain!(
        opt!(multispace) ~
        tag_s!(",") ~
        opt!(multispace), || {}
    ),
    symbolic_name
));

named!(path_expression<&str, ASTPath>, chain!(
    opt!(multispace) ~
    head: node_expression ~
    opt!(multispace) ~
    tail: many0!(chain!(
        edge: edge_expression ~
        opt!(multispace) ~
        node: node_expression ~
        opt!(multispace), ||
        {
            (edge, node)
        }
    )), || {
        ASTPath{head: head, tail: tail}
    }
));

pub enum ParseError {
    Problem
}

pub fn parse_expression(expression: &str) -> Result<ASTPath, ParseError> {
    match path_expression(expression) {
        IResult::Done(remaining, path) => {
            if remaining.len() == 0 {
                Ok(path)
            } else {
                Err(ParseError::Problem)
            }
        },
        _ => {
            Err(ParseError::Problem)
        }
    }
}

#[cfg(test)]
mod test {
    use super::{node_expression, edge_expression, path_expression};
    use nom::IResult;
    use queries::ast::{ASTEdge, ASTPath};
    use ::{PropVal};
    use matching::NodeMatcher;

    #[test]
    fn test_node_expression() {
        assert_eq!(node_expression("( )"), IResult::Done(&""[..], NodeMatcher::Any));
        assert_eq!(node_expression("(2 )"), IResult::Done(&""[..], NodeMatcher::Id(2)));
        assert_eq!(node_expression("({})"), IResult::Done(&""[..], NodeMatcher::Any));
        assert_eq!(node_expression("({a:5})"), IResult::Done(&""[..], NodeMatcher::PropVal("a".to_string().into_boxed_str(), PropVal::Int(5))));
        assert_eq!(node_expression("( { `a longer name` : \"Some string\" } )"), IResult::Done(&""[..],
            NodeMatcher::PropVal("a longer name".to_string().into_boxed_str(), PropVal::String("Some string".to_string().into_boxed_str()))));
        assert_eq!(node_expression("({a:1, b: 2, c: \"str\"})"), IResult::Done(&""[..],
            NodeMatcher::And(
                Box::new(NodeMatcher::And(
                    Box::new(NodeMatcher::PropVal("a".to_string().into_boxed_str(), PropVal::Int(1))),
                    Box::new(NodeMatcher::PropVal("b".to_string().into_boxed_str(), PropVal::Int(2))))),
                Box::new(NodeMatcher::PropVal("c".to_string().into_boxed_str(), PropVal::String("str".to_string().into_boxed_str()))))));
    }

    #[test]
    fn test_edge_expression() {
        assert_eq!(edge_expression("->"), IResult::Done(&""[..], ASTEdge::Any));
        assert_eq!(edge_expression("-label1>"), IResult::Done(&""[..], ASTEdge::LabelList(vec!["label1"])));
        assert_eq!(edge_expression("-label1, label2>"), IResult::Done(&""[..], ASTEdge::LabelList(vec!["label1", "label2"])));
        assert_eq!(edge_expression("- `a long label`     , label >"), IResult::Done(&""[..], ASTEdge::LabelList(vec!["a long label", "label"])));
    }

    #[test]
    fn test_path_expression() {
        assert_eq!(path_expression("(1) -> (2)"), IResult::Done(&""[..], ASTPath{head: NodeMatcher::Id(1), tail: vec![(ASTEdge::Any, NodeMatcher::Id(2))]}));
        assert_eq!(path_expression("() -> () -label> ()"), IResult::Done(&""[..], ASTPath{head: NodeMatcher::Any, tail: vec![(ASTEdge::Any, NodeMatcher::Any), (ASTEdge::LabelList(vec!["label"]), NodeMatcher::Any)]}));
    }
}
