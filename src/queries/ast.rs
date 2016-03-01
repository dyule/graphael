use ::PropVal;
use matching::NodeMatcher;
#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode<'a> {
    NodeIDExpression(usize),
    NodePropVal(&'a str, PropVal),
    NodeProp(&'a str),
    NodePropsList(Vec<ASTNode<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum ASTEdge<'a> {
    LabelList(Vec<&'a str>),
    Any
}

#[derive(Debug, PartialEq)]
pub struct ASTPath<'a> {
    pub head: NodeMatcher,
    pub tail: Vec<(ASTEdge<'a>, NodeMatcher)>
}
