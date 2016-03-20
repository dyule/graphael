use ::{NodeIndex, Node, PropVal};
use std::ops::Deref;
use std::collections::{HashSet};
mod automata;
mod results;
pub use ::matching::automata::MatchingAutomaton;
pub use ::matching::results::MatchResult;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum NodeMatcher {
    Any,
    Prop(Box<str>),
    PropVal(Box<str>, PropVal),
    Id(NodeIndex),
    And(Box<NodeMatcher>, Box<NodeMatcher>),
    Or(Box<NodeMatcher>, Box<NodeMatcher>)
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
pub enum EdgeMatcher<'a> {
    Any,
    LabelList(Vec<&'a str>)
}

pub static ANY: NodeMatcher =  NodeMatcher::Any;

pub enum PathMatcher<'a> {
    Labels(Vec<&'a str>, NodeMatcher),
    Repeat(Option<u32>, Option<u32>, Box<PathMatcher<'a>>),
    ThenLabels(Vec<&'a str>, Box<PathMatcher<'a>>),
    To(NodeMatcher, Box<PathMatcher<'a>>),
    Or(Box<PathMatcher<'a>>, Box<PathMatcher<'a>>)
}



pub fn node_with_prop(prop: &str) -> NodeMatcher {
    NodeMatcher::Prop(prop.to_string().into_boxed_str())
}

pub fn node_with_prop_val(prop: &str, val: PropVal) -> NodeMatcher {
    NodeMatcher::PropVal(prop.to_string().into_boxed_str(), val)
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
        PathMatcher::Labels(vec![label], self)
    }

    pub fn connected_by_one_of<'a>(self, labels: Vec<&'a str>) -> PathMatcher<'a> {
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

impl<'a> EdgeMatcher<'a> {
    pub fn matches(&self, labels: &HashSet<Box<str>>) -> bool {
        match *self {
            EdgeMatcher::Any => true,
            EdgeMatcher::LabelList(ref my_labels) => {
                for my_label in my_labels.iter() {
                    let my_label = my_label.to_string().into_boxed_str();
                    if labels.contains(&my_label) {
                        return true
                    }
                }
                false
            }
        }
    }
}

impl<'a> PathMatcher<'a> {
    pub fn repeat(self, min: Option<u32>, max: Option<u32>) -> PathMatcher<'a> {
        PathMatcher::Repeat(min, max, Box::new(self))
    }

    pub fn then_label(self, label: &'a str) -> PathMatcher {
        PathMatcher::ThenLabels(vec![label], Box::new(self))
    }

    pub fn then_one_of(self, labels: Vec<&'a str>) -> PathMatcher<'a> {
        PathMatcher::ThenLabels(labels, Box::new(self))
    }

    pub fn to(self, node: NodeMatcher) -> PathMatcher<'a> {
        PathMatcher::To(node, Box::new(self))
    }
}

#[cfg(test)]
mod tests {
    use super::{node_with_prop};
    use ::{PropVal, GraphDB, Graph};
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
        let mut g = GraphDB::new();
        let n1 = g.add_node_with_props(dict!{"prop1" => "val1", "prop2" => "val2"});
        let n2 = g.add_node_with_props(dict!{"prop1" =>  "val1", "prop2" => "val3"});
        let n3 = g.add_node_with_props(dict!{"prop2" =>  "val2"});
        assert!(node_matcher.matches(g.get_node(n1).unwrap()));
        assert!(!node_matcher.matches(g.get_node(n2).unwrap()));
        assert!(!node_matcher.matches(g.get_node(n3).unwrap()));

    }

}
