//! Graphael is a lightweight graph database suitable for embedding in other applications.
extern crate rustc_serialize;
#[macro_use]
extern crate nom;

use std::collections::{HashMap, HashSet};

pub mod database;
pub mod matching;
pub mod queries;
use matching::MatchingAutomaton;
use queries::ParseError;
use database::PropIndexEntry;
use std::sync::{Arc, RwLock, Weak};

/// Holds the data and metadata for this database.
///
/// A node can have any number of properties in any fashion (i.e. there is no schema)
///
///
/// # Example
///
/// ```
/// # use graphael::{GraphDB, PropVal};
/// let mut graph = GraphDB::new();
/// let id = graph.add_node();
/// let node = graph.get_node_mut(id).unwrap();
/// node.set_prop("this_prop", PropVal::Int(5));
/// ```

#[derive(Debug, Clone)]
pub struct Node {
    ///The ID of this node.  Should not be changed
    id: NodeIndex,

    /// The graph this node belongs to
    graph: Weak<RwLock<GraphInternal>>,

    /// The properties associated with this node.  The boxed string datatype can be generated from a string literal via `"property".to_string().into_boxed_str()`
    props:HashMap<Box<str>, PropVal>,
}

/**************************/
/*** Struct definitions ***/
/**************************/



/// Connects nodes together with labeled relationships.
#[derive(Debug, PartialEq, Clone)]
pub struct Edge {
    /// The names of the relationships between nodes.
	labels: HashSet<Box<str>>
}

/// Represents a value in a property field.
///
/// # Examples
///
/// ```
/// # use graphael::{GraphDB, PropVal};
/// # let mut graph = GraphDB::new();
/// # let id = graph.add_node();
/// # let node = graph.get_node_mut(id).unwrap();
/// node.set_prop("this_prop", PropVal::Int(5));
/// node.set_prop("another_prop", PropVal::String("a value".to_string().into_boxed_str()));
/// println!("The property is: {}", node.get_prop("this_prop").unwrap());
/// ```
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PropVal {
    /// An integer value
	Int(i64),
    /// A string value.  Can be converted from a string literal via `"value".to_string().into_boxed_str()`
	String(Box<str>)
}

#[derive (Debug)]
pub struct GraphDB {
    internal: Arc<RwLock<GraphInternal>>,
    nodes: HashMap<NodeIndex, Node>,
    edges: HashMap<NodeIndex, HashMap<NodeIndex, Edge>>,
    reverse_edges: HashMap<NodeIndex, HashSet<NodeIndex>>,
    max_node_id: NodeIndex
}

#[derive (Debug)]
struct GraphInternal {
    prop_index: HashMap<Box<str>, PropIndexEntry>,
    label_index: HashMap<Box<str>, HashSet<(NodeIndex, NodeIndex)>>,
}

pub type NodeIndex = usize;

pub trait Graph {
    fn get_node(&self, node_id: NodeIndex) -> Option<&Node>;

    fn nodes_with_prop(&self, key: &str, value: &PropVal) -> Vec<NodeIndex>;

    fn are_connected(&mut self, origin: NodeIndex, destination: NodeIndex) -> bool;

    fn edges_from(&self, source: NodeIndex) -> &HashMap<NodeIndex, Edge>;

    fn edges_with_label(&self, label: &str) -> HashSet<(NodeIndex, NodeIndex)>;

    fn edges_with_label_from(&self, source: NodeIndex, label: &str) -> Vec<NodeIndex>;
}

pub trait IntoAutomata<'a> {
    fn into_automata(self) -> Result<MatchingAutomaton<'a>, ParseError>;
}
