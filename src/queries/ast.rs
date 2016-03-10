use ::PropVal;
use matching::NodeMatcher;
#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode<'a> {
    NodeIDExpression(usize),
    NodePropVal(&'a str, PropVal),
    NodeProp(&'a str),
    NodePropsList(Vec<ASTNode<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTEdge<'a> {
    LabelList(Vec<&'a str>),
    Any
}

#[derive(Debug, PartialEq)]
pub struct ASTPath<'a> {
    pub head: NodeMatcher,
    pub tail: EdgeToNode<'a>
}

#[derive(Clone, Debug, PartialEq)]
pub enum EdgeToNode<'a> {
    Standard {
        edge: ASTEdge<'a>,
        node: NodeMatcher,
        tail: Option<Box<EdgeToNode<'a>>>
    },
    Repeated {
        tail: Option<Box<EdgeToNode<'a>>>,
        repeated: Box<EdgeToNode<'a>>,
        min: u32,
        max: Option<u32>
    }
}

impl<'a> EdgeToNode<'a> {
    pub fn get_tail(&mut self) -> & mut Option<Box<EdgeToNode<'a>>> {
        match self {
            &mut EdgeToNode::Standard {ref mut tail, ..} => tail,
            &mut EdgeToNode::Repeated {ref mut tail, ..} => tail
        }
    }
}
