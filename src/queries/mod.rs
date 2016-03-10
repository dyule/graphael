use nom::{multispace, IResult};
pub use queries::ast::{ASTNode, ASTEdge, ASTPath, EdgeToNode};
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


named!(edge_to_node<&str, EdgeToNode >, alt!(
    chain!(
        edge: edge_expression ~
        opt!(multispace) ~
        node: node_expression  ~
        opt!(multispace) ~
        tail: opt!(complete!(edge_to_node)), ||
        {
            EdgeToNode::Standard {
                edge: edge,
                node: node,
                tail: if let Some(ref tail) = tail {Some(Box::new(tail.clone()))} else {None}
            }
        }
   ) |chain!(
       head: delimited!(
            tag_s!("["),
            complete!(edge_to_node),
            tag_s!("]")
        ) ~
        repeater: opt!(complete!(repeat_brace)) ~
        opt!(multispace) ~
        tail: opt!(complete!(edge_to_node)), ||
        {
            match repeater {
                Some((min, max)) => {
                    EdgeToNode::Repeated {
                        min: min,
                        max: max,
                        repeated: Box::new(head),
                        tail: if let Some(ref tail) = tail {Some(Box::new(tail.clone()))} else {None}
                    }
                }
                None => {
                    let mut head = head.clone();
                    if head.get_tail().is_some() {
                        let mut current = &mut head;
                        while let &mut Some(ref mut current) = current.get_tail() {
                            if current.get_tail().is_none() {
                                *current.get_tail() = if let Some(ref tail) = tail {Some(Box::new(tail.clone()))} else {None};
                                break;
                            }
                        }
                    } else {
                        *head.get_tail() = if let Some(ref tail) = tail {Some(Box::new(tail.clone()))} else {None};
                    }
                    head
                }
            }
        }
    )

));

named!(unsiged_integer<&str, u32>, map_res!(
    take_while_s!(is_digit),
    |num:&str| {
        u32::from_str(num)
}));

named!(repeat_brace<&str, (u32, Option<u32>)>, delimited!(
    tag_s!("{"),
    chain!(
        opt!(multispace) ~
        min: opt!(unsiged_integer) ~
        opt!(multispace) ~
        tag_s!(",") ~
        opt!(multispace) ~
        max: opt!(unsiged_integer) ~
        opt!(multispace), || {
            let min = match min {
                Some(min) => min,
                None => 0
            };
            (min, max)
        }
    ),
    tag_s!("}")
));

named!(path_expression<&str, ASTPath>, chain!(
    opt!(multispace) ~
    head: node_expression ~
    opt!(multispace) ~
    tail: edge_to_node, || {
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
    use super::{node_expression, edge_expression, path_expression, edge_to_node, EdgeToNode};
    use nom::IResult;
    use queries::ast::{ASTEdge, ASTPath};
    use ::{PropVal};
    use matching::NodeMatcher;

    macro_rules! assert_result{($left: expr, $right: expr) => {
        assert_eq!($left, IResult::Done(&""[..], $right));
    }}

    #[test]
    fn parse_node_expression() {
        assert_result!(node_expression("( )"), NodeMatcher::Any);
        assert_result!(node_expression("(2 )"), NodeMatcher::Id(2));
        assert_result!(node_expression("({})"), NodeMatcher::Any);
        assert_result!(node_expression("({a:5})"), NodeMatcher::PropVal("a".to_string().into_boxed_str(), PropVal::Int(5)));
        assert_result!(node_expression("( { `a longer name` : \"Some string\" } )"),
            NodeMatcher::PropVal("a longer name".to_string().into_boxed_str(), PropVal::String("Some string".to_string().into_boxed_str())));
        assert_result!(node_expression("({a:1, b: 2, c: \"str\"})"),
            NodeMatcher::And(
                Box::new(NodeMatcher::And(
                    Box::new(NodeMatcher::PropVal("a".to_string().into_boxed_str(), PropVal::Int(1))),
                    Box::new(NodeMatcher::PropVal("b".to_string().into_boxed_str(), PropVal::Int(2))))),
                Box::new(NodeMatcher::PropVal("c".to_string().into_boxed_str(), PropVal::String("str".to_string().into_boxed_str())))));
    }

    #[test]
    fn parse_edge_expression() {
        assert_result!(edge_expression("->"), ASTEdge::Any);
        assert_result!(edge_expression("-label1>"), ASTEdge::LabelList(vec!["label1"]));
        assert_result!(edge_expression("-label1, label2>"), ASTEdge::LabelList(vec!["label1", "label2"]));
        assert_result!(edge_expression("- `a long label`     , label >"), ASTEdge::LabelList(vec!["a long label", "label"]));
    }

    #[test]
    fn parse_path_expression() {
        assert_result!(path_expression("(1) -> (2)"), ASTPath{head: NodeMatcher::Id(1), tail: EdgeToNode::Standard{edge: ASTEdge::Any, node: NodeMatcher::Id(2), tail: None}});
        assert_result!(path_expression("() -> () -label> ()"), ASTPath{
            head: NodeMatcher::Any,
            tail: EdgeToNode::Standard {
                edge: ASTEdge::Any,
                node: NodeMatcher::Any,
                tail:Some(Box::new(EdgeToNode::Standard {
                    edge: ASTEdge::LabelList(vec!["label"]),
                    node: NodeMatcher::Any,
                    tail: None
                }))
            }
        });

    }

    #[test]
    fn parse_edge_to_node() {
        assert_result!(edge_to_node("-> ()"), EdgeToNode::Standard {
            edge: ASTEdge::Any,
            node: NodeMatcher::Any,
            tail: None
        });

        assert_result!(edge_to_node("-> (16) -> ()"), EdgeToNode::Standard {
            edge: ASTEdge::Any,
            node: NodeMatcher::Id(16),
            tail: Some(Box::new(
                EdgeToNode::Standard {
                    edge: ASTEdge::Any,
                    node: NodeMatcher::Any,
                    tail: None
                }
            ))
        });

        assert_result!(edge_to_node("[-> ()] -> ()"), EdgeToNode::Standard {
            edge: ASTEdge::Any,
            node: NodeMatcher::Any,
            tail: Some(Box::new(
                EdgeToNode::Standard {
                    edge: ASTEdge::Any,
                    node: NodeMatcher::Any,
                    tail: None
                }
            ))
        });

        assert_result!(edge_to_node("-> () [-> () [[-> ()]]]"),  EdgeToNode::Standard {
            edge: ASTEdge::Any,
            node: NodeMatcher::Any,
            tail: Some(Box::new(
                EdgeToNode::Standard {
                    edge: ASTEdge::Any,
                    node: NodeMatcher::Any,
                    tail: Some(Box::new(EdgeToNode::Standard {
                        edge: ASTEdge::Any,
                        node: NodeMatcher::Any,
                        tail: None
                    }))
                }
            ))
        });

        assert_result!(edge_to_node("-> () [-> () -> ()] -> ()"), EdgeToNode::Standard {
            edge: ASTEdge::Any,
            node: NodeMatcher::Any,
            tail: Some(Box::new(
                EdgeToNode::Standard {
                    edge: ASTEdge::Any,
                    node: NodeMatcher::Any,
                    tail: Some(Box::new(
                        EdgeToNode::Standard {
                            edge: ASTEdge::Any,
                            node: NodeMatcher::Any,
                            tail: Some(Box::new(
                                EdgeToNode::Standard {
                                    edge: ASTEdge::Any,
                                    node: NodeMatcher::Any,
                                    tail: None
                                }
                            ))
                        }
                    ))
                }
            ))
        });


        assert_result!(edge_to_node("[-> ()]{1, 2}"), EdgeToNode::Repeated{
            min: 1,
            max: Some(2),
            repeated: Box::new(
                EdgeToNode::Standard {
                    edge: ASTEdge::Any,
                    node: NodeMatcher::Any,
                    tail: None
                }
            ),
            tail: None
        });

        assert_result!(edge_to_node("[-> ()]{1, } -> ()"), EdgeToNode::Repeated{
            min: 1,
            max: None,
            repeated: Box::new(
                EdgeToNode::Standard {
                    edge: ASTEdge::Any,
                    node: NodeMatcher::Any,
                    tail: None
                }
            ),
            tail: Some(Box::new(
                EdgeToNode::Standard {
                    edge: ASTEdge::Any,
                    node: NodeMatcher::Any,
                    tail: None
                }
            ))
        });

        assert_result!(edge_to_node("[[-> ()]-> ()]{,} -> ()"), EdgeToNode::Repeated{
            min: 0,
            max: None,
            repeated: Box::new(
                EdgeToNode::Standard {
                    edge: ASTEdge::Any,
                    node: NodeMatcher::Any,
                    tail: Some(Box::new(
                        EdgeToNode::Standard {
                            edge: ASTEdge::Any,
                            node: NodeMatcher::Any,
                            tail: None
                        }
                    ))
                }
            ),
            tail: Some(Box::new(
                EdgeToNode::Standard {
                    edge: ASTEdge::Any,
                    node: NodeMatcher::Any,
                    tail: None
                }
            ))
        });
    }

}
