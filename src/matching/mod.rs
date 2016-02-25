use ::{NodeIndex, Node, PropVal};
use std::ops::Deref;
mod automata;
mod results;
pub use ::matching::automata::MatchingAutomaton;
pub use ::matching::results::MatchResult;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum NodeMatcher {
    Any,
    Prop(Box<str>),
    PropVal(Box<str>, Box<PropVal>),
    Id(NodeIndex),
    And(Box<NodeMatcher>, Box<NodeMatcher>),
    Or(Box<NodeMatcher>, Box<NodeMatcher>)
}

pub static ANY: NodeMatcher =  NodeMatcher::Any;

pub enum PathMatcher {
    Labels(Vec<Box<str>>, NodeMatcher),
    Repeat(Option<u32>, Option<u32>, Box<PathMatcher>),
    ThenLabels(Vec<Box<str>>, Box<PathMatcher>),
    To(NodeMatcher, Box<PathMatcher>),
    Or(Box<PathMatcher>, Box<PathMatcher>)
}



pub fn node_with_prop(prop: &str) -> NodeMatcher {
    NodeMatcher::Prop(prop.to_string().into_boxed_str())
}

pub fn node_with_prop_val(prop: &str, val: PropVal) -> NodeMatcher {
    NodeMatcher::PropVal(prop.to_string().into_boxed_str(), Box::new(val))
}

pub fn node_with_id(id: NodeIndex) -> NodeMatcher {
    NodeMatcher::Id(id)
}

impl NodeMatcher {
    pub fn and_prop(self, prop: &str) -> NodeMatcher {
        NodeMatcher::And(Box::new(node_with_prop(prop)), Box::new(self))
    }

    pub fn or_prop(self, prop: &str) -> NodeMatcher {
        NodeMatcher::Or(Box::new(node_with_prop(prop)), Box::new(self))
    }

    pub fn and_prop_val(self, prop: &str, val: PropVal) -> NodeMatcher {
        NodeMatcher::And(Box::new(node_with_prop_val(prop, val)), Box::new(self))
    }

    pub fn or_prop_val(self, prop: &str, val: PropVal) -> NodeMatcher {
        NodeMatcher::Or(Box::new(node_with_prop_val(prop, val)), Box::new(self))
    }

    pub fn connected_by_label(self, label: &str) -> PathMatcher {
        PathMatcher::Labels(vec![label.to_string().into_boxed_str()], self)
    }

    pub fn connected_by_one_of(self, labels: Vec<Box<str>>) -> PathMatcher {
        PathMatcher::Labels(labels, self)
    }

    pub fn matches(&self, node: &Node) -> bool {
        match *self {
            NodeMatcher::Prop(ref prop) => node.props.contains_key(prop),
            NodeMatcher::PropVal(ref prop, ref val) => match node.props.get(prop) {
                Some(v) => v == val.deref(),
                None => false
            },
            NodeMatcher::Id(id) => node.id == id,
            NodeMatcher::And(ref lhs, ref rhs) => lhs.matches(node) && rhs.matches(node),
            NodeMatcher::Or(ref lhs, ref rhs) => lhs.matches(node) || rhs.matches(node),
            NodeMatcher::Any => true
        }
    }

}

impl PathMatcher {
    pub fn repeat(self, min: Option<u32>, max: Option<u32>) -> PathMatcher {
        PathMatcher::Repeat(min, max, Box::new(self))
    }

    pub fn then_label(self, label: &str) -> PathMatcher {
        PathMatcher::ThenLabels(vec![label.to_string().into_boxed_str()], Box::new(self))
    }

    pub fn then_one_of(self, labels: Vec<Box<str>>) -> PathMatcher {
        PathMatcher::ThenLabels(labels, Box::new(self))
    }

    pub fn to(self, node: NodeMatcher) -> PathMatcher {
        PathMatcher::To(node, Box::new(self))
    }
}

#[cfg(test)]
mod tests {
    use super::{node_with_prop, node_with_id};
    use ::{PropVal, Graph};
    macro_rules! dict (
    	{ $($key:expr => $value:expr),+ } => {
            {
                let mut dict = ::std::collections::HashMap::new();
                $(
                    dict.insert($key.to_string().into_boxed_str(), PropVal::String($value.to_string().into_boxed_str()));
                )+
                dict
            }
         };
    );
    #[test]
    fn test_node_matcher() {
        let node_matcher = node_with_prop("prop1").and_prop_val("prop2", PropVal::String("val2".to_string().into_boxed_str()));
        let mut g = Graph::new();
        let n1 = g.add_node_with_props(dict!{"prop1" => "val1", "prop2" => "val2"});
        let n2 = g.add_node_with_props(dict!{"prop1" =>  "val1", "prop2" => "val3"});
        let n3 = g.add_node_with_props(dict!{"prop2" =>  "val2"});
        assert!(node_matcher.matches(g.get_node(n1).unwrap()));
        assert!(!node_matcher.matches(g.get_node(n2).unwrap()));
        assert!(!node_matcher.matches(g.get_node(n3).unwrap()));

    }

}
