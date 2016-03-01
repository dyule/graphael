//! Graphael is a lightweight graph database suitable for embedding in other applications.
extern crate rustc_serialize;
#[macro_use]
extern crate nom;
use std::collections::{HashMap, HashSet, LinkedList, VecDeque};
use std::hash::{Hash, Hasher};
use std::collections::hash_map::Entry;
use rustc_serialize::{Decoder, Decodable, Encoder, Encodable};
use rustc_serialize::json::{self, Json, ToJson};
use std::fs::File;
use std::io::{Read, Write};
use std::io::Result as IOResult;
use std::string::ToString;
use matching::*;

pub mod queries;
pub mod matching;
/**************************/
/*** Struct definitions ***/
/**************************/

/// Holds the data and metadata for this database.
///
/// A node can have any number of properties in any fashion (i.e. there is no schema)
///
///
/// # Example
///
///
/// ```
/// # use graphael::{Graph, PropVal};
/// let mut graph = Graph::new();
/// let id = graph.add_node();
/// let node = graph.get_node_mut(id).unwrap();
/// node.props.insert("this_prop".to_string().into_boxed_str(), PropVal::Int(5));
/// ```

#[derive(PartialEq, Debug)]
pub struct Node {
    ///The ID of this node.  Should not be changed
    id: NodeIndex,

    /// The properties associated with this node.  The boxed string datatype can be generated from a string literal via `"property".to_string().into_boxed_str()`
    pub props:HashMap<Box<str>, PropVal>,
}


/// Connects nodes together with labeled relationships.  Will soon be removed from the public interface
#[derive(Debug, PartialEq, Clone)]
pub struct Edge {
    /// The names of the relationships between nodes.
	pub labels: HashSet<Box<str>>
}

#[derive(Debug)]
pub struct DAG<'a> {
    roots: HashSet<NodeIndex>,
    nodes: HashMap<NodeIndex, DAGNode<'a>>
}

#[derive(Debug)]
pub struct DAGNode<'a> {
    node: &'a Node,
    connected_to: Vec<NodeIndex>,
}

#[derive(Debug, Clone)]
pub struct Path<'a> {
	pub nodes: Vec<NodeIndex>,
	pub edges: Vec<&'a str>

}

/// Represents a value in a property field.
///
/// # Examples
///
/// ```
/// # use graphael::{Graph, PropVal};
/// # let mut graph = Graph::new();
/// # let id = graph.add_node();
/// # let node = graph.get_node_mut(id).unwrap();
/// node.props.insert("this_prop".to_string().into_boxed_str(), PropVal::Int(5));
/// node.props.insert("another_prop".to_string().into_boxed_str(), PropVal::String("a value".to_string().into_boxed_str()));
/// ```
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum PropVal {
    /// An integer value
	Int(i64),
    /// A string value.  Can be converted from a string literal via `"value".to_string().into_boxed_str()`
	String(Box<str>)
}

#[derive (Debug)]
pub struct Graph {
    nodes: HashMap<NodeIndex, Node>,
    edges: HashMap<NodeIndex, HashMap<NodeIndex, Edge>>,
    reverse_edges: HashMap<NodeIndex, HashSet<NodeIndex>>,
    max_node_id: NodeIndex
}

pub type NodeIndex = usize;

/***********************/
/*** Implementations ***/
/***********************/

impl  PartialEq for Graph {
    fn eq(&self, other:&Self) -> bool {
        return self.nodes.eq(&other.nodes) && self.edges.eq(&other.edges)
    }
}

// impl ToJson for PropVal {
// 	fn to_json(&self) -> Json {
// 		match *self {
// 			PropVal::String(ref s) => s.to_json(),
// 			PropVal::Int(i) => i.to_json()
// 		}
// 	}
// }
//
// /// Used to convert a Node to JSON
// impl ToJson for Node {
// 	fn to_json(&self) -> Json {
// 		let mut d = BTreeMap::new();
// 		let mut props = BTreeMap::new();
// 		d.insert("id".to_string(), self.id.to_json());
// 		for (k, v) in self.props.iter() {
// 			props.insert(k.to_string(), v.to_json());
// 		}
// 		d.insert("props".to_string(), Json::Object(props));
// 		Json::Object(d)
// 	}
// }
//
// impl ToJson for Edge {
// 	fn to_json(&self) -> Json {
// 		let mut d = BTreeMap::new();
// 		d.insert("labels".to_string(), Json::Array(self.labels.iter().map(|l| Json::String(l.to_string())).collect()));
// 		Json::Object(d)
// 	}
// }
impl <'a> ToJson for Graph {
	fn to_json(&self) -> Json {
		// let mut d = BTreeMap::new();
		// let mut edge_json_map = BTreeMap::new();
		// d.insert("nodes".to_string(), Json::Object(self.nodes.iter().map(|(i, n)| (i.to_string(), n.to_json())).collect()));
		// for (index, edge) in self.edges.iter() {
		// 	edge_json_map.insert(index.to_string(), Json::Object(edge.iter().map(|(i, n)| (i.to_string(), n.to_json())).collect()));
		// }
		// d.insert("edges".to_string(), Json::Object(edge_json_map));
		// Json::Object(d)
        Json::from_str(&json::encode(self).unwrap()).unwrap()
    }
}

impl Encodable for PropVal {
    fn encode<E:Encoder>(&self, encoder: &mut E) -> Result<(), E::Error> {

        encoder.emit_struct("root", 2, |encoder| {
            match *self {
                PropVal::Int(ref val) => {
                    try!(encoder.emit_struct_field("type", 0, |encoder| {
                        encoder.emit_usize(0)
                    }));
                    encoder.emit_struct_field("value", 1, |encoder| {
                        encoder.emit_i64(*val)
                    })
                },
                PropVal::String(ref val) => {
                    try!(encoder.emit_struct_field("type", 0, |encoder| {
                        encoder.emit_usize(0)
                    }));
                    encoder.emit_struct_field("value", 1, |encoder| {
                        encoder.emit_str(val)
                    })
                },
            }
        })
    }
}

impl Encodable for Node {
    fn encode<E:Encoder>(&self, encoder: &mut E) -> Result<(), E::Error> {
        encoder.emit_struct("root", 2, |encoder| {
            try!(encoder.emit_struct_field("id", 0, |encoder| {
                encoder.emit_usize(self.id)
            }));
            encoder.emit_struct_field("props", 1, |encoder| {
                encoder.emit_map(self.props.len(), |encoder| {
                    for (index, (key, val)) in self.props.iter().enumerate() {
                        try!(encoder.emit_map_elt_key(index, |encoder| {encoder.emit_str(key)}));
                        try!(encoder.emit_map_elt_val(index, |encoder| {val.encode(encoder)}));
                    }
                    Ok(())
                })
            })
        })
    }
}

impl Encodable for Edge {
    fn encode<E:Encoder>(&self, encoder: &mut E) -> Result<(), E::Error> {
        encoder.emit_struct("root", 1, |encoder| {
            encoder.emit_struct_field("labels", 0, |encoder| {
                encoder.emit_seq(self.labels.len(), |encoder| {
                    for (index, val) in self.labels.iter().enumerate() {
                        try!(encoder.emit_seq_elt(index, |encoder| {
                            encoder.emit_str(val)
                        }));
                    }
                    Ok(())
                })
            })
        })
    }
}

impl Encodable for Graph {
    fn encode<E:Encoder>(&self, encoder: &mut E) -> Result<(), E::Error> {
        encoder.emit_struct("root", 2, |encoder| {
            try!(encoder.emit_struct_field("nodes", 0, |encoder| {
                encoder.emit_map(self.nodes.len(), |encoder| {
                    for (index, (node_id, node)) in self.nodes.iter().enumerate() {
                        try!(encoder.emit_map_elt_key(index, |encoder| {encoder.emit_usize(*node_id)}));
                        try!(encoder.emit_map_elt_val(index, |encoder| {node.encode(encoder)}));
                    }
                    Ok(())
                })
            }));
            encoder.emit_struct_field("edges", 1, |encoder| {
                encoder.emit_map(self.edges.len(), |encoder| {
                    for (index, (source, edge_map)) in self.edges.iter().enumerate() {
                        try!(encoder.emit_map_elt_key(index, |encoder| {encoder.emit_usize(*source)}));
                        try!(encoder.emit_map_elt_val(index, |encoder| {
                            encoder.emit_map(edge_map.len(), |encoder| {
                                for (index, (target, edge)) in edge_map.iter().enumerate() {
                                    try!(encoder.emit_map_elt_key(index, |encoder| {encoder.emit_usize(*target)}));
                                    try!(encoder.emit_map_elt_val(index, |encoder| {edge.encode(encoder)}));
                                }
                                Ok(())
                            })
                        }));
                    }
                    Ok(())
                })
            })
        })
    }
}

struct Link {
    source: NodeIndex,
    target: NodeIndex
}

impl<'a> Encodable for DAG<'a> {
    fn encode<E:Encoder>(&self, encoder: &mut E) -> Result<(), E::Error> {
        let mut node_array = Vec::new();
        let mut link_array = Vec::new();
        let mut id_lookup = HashMap::new();

        for (index, (id, node)) in self.nodes.iter().enumerate() {
            node_array.push(node.node);
            id_lookup.insert(id, index);
        }
        for (id, node) in self.nodes.iter() {
            let id = id_lookup.get(id).unwrap();
            for target in node.connected_to.iter() {
                link_array.push(Link{source: id.clone(), target: id_lookup[target].clone()})
            }
        }
        encoder.emit_struct("root", 2, |encoder| {
            try!(encoder.emit_struct_field("nodes", 0, |encoder| {
                // encoder.emit_seq(node_array.len(), |encoder| {
                //     for (index, node) in node_array.iter().enumerate() {
                //         try!(encoder.emit_seq_elt(index, |encoder| {
                //             node.encode(encoder)
                //         }));
                //     }
                //     Ok(())
                // })
                node_array.encode(encoder)
            }));
            encoder.emit_struct_field("links", 1, |encoder| {
                // encoder.emit_seq(link_array.len(), |encoder| {
                //     for (index, link) in link_array.iter().enumerate() {
                //         try!(encoder.emit_seq_elt(index, |encoder| {
                //             link.encode(encoder)
                //         }));
                //     }
                //     Ok(())
                // })
                link_array.encode(encoder)
            })
        })
    }
}

impl Encodable for Link {
    fn encode<E:Encoder>(&self, encoder: &mut E) -> Result<(), E::Error> {
        encoder.emit_struct("root", 2, |encoder| {
            try!(encoder.emit_struct_field("source", 0, |encoder| {
                encoder.emit_usize(self.source)
            }));
            encoder.emit_struct_field("target", 1, |encoder| {
                encoder.emit_usize(self.target)
            })
        })
    }
}

impl Decodable for PropVal {
    fn decode<D:Decoder>(decoder: &mut D) -> Result<Self, D::Error> {
        decoder.read_struct("root", 2, |decoder| {
            let t = try!(decoder.read_struct_field("type", 0, |decoder| {
                decoder.read_usize()
            }));
            decoder.read_struct_field("value", 1, |decoder| {
                match t {
                    0 => match decoder.read_i64() {
                        Ok(val) => Ok(PropVal::Int(val)),
                        Err(e) => Err(e)
                    },
                    1 => match decoder.read_str() {
                        Ok(val) => Ok(PropVal::String(val.into_boxed_str())),
                        Err(e) => Err(e)
                    },
                    _ => Err(decoder.error("Unexpected property value type"))
                }
            })
        })
    }
}

impl Decodable for Node {
    fn decode<D:Decoder>(decoder: &mut D) -> Result<Self, D::Error> {
        decoder.read_struct("root", 2, |decoder| {
            let id = try!(decoder.read_struct_field("id", 0, |decoder| {
                decoder.read_usize()
            }));
            let props = try!(decoder.read_struct_field("props", 1, |decoder| {
                decoder.read_map(|decoder, len| {
                    let mut props:HashMap<Box<str>, PropVal> = HashMap::new();
                    for idx in 0..len {
                        let prop_key:String = try!(decoder.read_map_elt_key(idx, |decoder| decoder.read_str()));
                        let value:PropVal = try!(decoder.read_map_elt_val(idx, |decoder| PropVal::decode(decoder)));
                        props.insert(prop_key.into_boxed_str(), value);
                    }
                    Ok(props)
                })
            }));
            Ok(Node{
                id: id,
                props: props
            })
        })
    }
}

impl Decodable for Edge {
    fn decode<D:Decoder>(decoder: &mut D) -> Result<Self, D::Error> {
        decoder.read_struct("root", 1, |decoder| {
            let labels = try!(decoder.read_struct_field("labels", 0, |decoder| {
                decoder.read_seq(|decoder, len| {
                    let mut labels:HashSet<Box<str>> = HashSet::new();
                    for idx in 0..len {
                        let label:String = try!(decoder.read_seq_elt(idx, |decoder| decoder.read_str()));
                        labels.insert(label.into_boxed_str());
                    }
                    Ok(labels)
                })
            }));
            Ok(Edge{
                labels: labels
            })
        })
    }
}

impl Decodable for Graph {

    fn decode<D:Decoder>(decoder: &mut D) -> Result<Self, D::Error> {
        decoder.read_struct("root", 2, |decoder| {
            let mut max_node_id = 0;
            let nodes = try!(decoder.read_struct_field("nodes", 1, |decoder| {
                decoder.read_map(|decoder, len| {
                    let mut nodes = HashMap::new();
                    for idx in 0..len {
                        let node_index = try!(decoder.read_map_elt_key(idx, |decoder| decoder.read_usize()));
                        let node:Node = try!(decoder.read_map_elt_val(idx, |decoder| Decodable::decode(decoder)));
                        if node_index > max_node_id {
                            max_node_id = node_index + 1;
                        }
                        nodes.insert(node_index, node);
                    }
                    Ok(nodes)
                })

        }));
        let mut g = Graph{
                max_node_id: max_node_id,
                nodes: nodes,
                edges: HashMap::new(),
                reverse_edges: HashMap::new()
            };
            try!(decoder.read_struct_field("edges", 2, |decoder| {
                decoder.read_map(|decoder, len| {
                    for idx in 0..len {
                        let source_index = try!(decoder.read_map_elt_key(idx, |decoder| decoder.read_usize()));
                        let edge_map:HashMap<NodeIndex, Edge> = try!(decoder.read_map_elt_val(idx, |decoder| Decodable::decode(decoder)));
                        for (destination_index, edge) in edge_map.iter() {
							for label in edge.labels.iter() {
                            	g.connect_nodes(source_index, *destination_index, label)
							}
                        };
                    }
                    Ok(())
                })
            }));
            Ok(g)
        })

    }
}

impl<'a> PartialEq<DAGNode<'a>> for DAGNode<'a> {
    fn eq(&self, other: &DAGNode) -> bool {
        self.node.id == other.node.id
    }
}

impl<'a> Eq for DAGNode<'a> {}

impl <'a> Hash for DAGNode<'a> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.node.id.hash(state)
    }
}

/*** Graph ***/

impl Graph {

	/// Creates a new graph with no nodes or edges
    pub fn new() -> Graph {
        Graph {
            max_node_id: 0,
            nodes: HashMap::new(),
            edges: HashMap::new(),
            reverse_edges: HashMap::new()
        }
    }

	/// Encode and decode from file

	pub fn from_json(json: Json) -> Graph {
		let mut decoder = json::Decoder::new(json);
		match Decodable::decode(&mut decoder) {
			Ok(x) => x,
			Err(e) => panic!("Could not decode to graph: {}", e)
		}
	}

	pub fn read_from_file(name: String) -> Graph {
		let mut contents = String::new();
		let mut file:File = File::open(name).unwrap();

		file.read_to_string(&mut contents).unwrap();
		let jsonstring = match Json::from_str(&contents) {
			Ok(a) => a,
			Err(e) => panic!("Error reading JSON string: {}", e)
		};

		Graph::from_json(jsonstring)
	}

	pub fn write_to_file(&self, name: &'static str) -> IOResult<usize> {
		let mut file = try!(File::create(name));
		file.write(self.to_json().to_string().as_bytes())
	}

	/*******************/
	/****** Nodes ******/
	/*******************/

	/// Gets the next node id
    fn get_node_next_id(&mut self) -> NodeIndex {
        self.max_node_id = self.max_node_id + 1;
        self.max_node_id
    }

	/// Adds a Node to the Graph
    pub fn add_node(&mut self) -> NodeIndex {
        let idx = self.get_node_next_id();
        let node:Node = Node {
            id: idx,
            props: HashMap::new(),
        };
        self.nodes.insert(idx, node);
		idx
    }

	/// Adds a Node to the Graph with the specified properties
	pub fn add_node_with_props(&mut self, props: HashMap<Box<str>, PropVal>) -> NodeIndex {
		//let id = self.add_node();
        let id = self.get_node_next_id();
        let node:Node = Node {
            id: id,
            props: props
        };
        self.nodes.insert(id, node);
		id
	}

	// Removes a Node from the Graph given a NodeIndex
    pub fn remove_node(&mut self, node_id:NodeIndex) {
        if !self.nodes.contains_key(&node_id) {
            panic!("Tried to remove a node that didn't exist: {}", node_id);
        }
        if let Some(re) = self.reverse_edges.get(&node_id) {
            for n in re {
                self.edges.get_mut(n).unwrap().remove(&node_id);
            }
        }
        if let Some(e) = self.edges.get(&node_id) {
            for n in e.keys() {
                self.reverse_edges.get_mut(n).unwrap().remove(&node_id);
            }
        }
        self.edges.remove(&node_id);
        self.reverse_edges.remove(&node_id);
        self.nodes.remove(&node_id);
    }

    pub fn get_node(&self, node_id:NodeIndex) -> Option<&Node> {
        self.nodes.get(&node_id)
    }

    pub fn get_node_mut(&mut self, node_id:NodeIndex) -> Option<&mut Node> {
        self.nodes.get_mut(&node_id)
    }

	/// Get Nodes with a given attribute
    pub fn nodes_with_attr(&self, attr: &str) -> Vec<&Node> {
		self.nodes.values().filter(|node|
			if let Some(e) = self.edges.get(&node.id) {
				if let Some(edge) = e.get(&node.id) {
					edge.labels.contains(attr)
				} else {
					false
				}
			}
			else {
				false
			}).collect()
    }

    /// Get Nodes with a key-value pair
    pub fn nodes_with_prop(&self, key: &str, value: &PropVal) -> Vec<NodeIndex> {
        self.nodes.values().filter(|node|
            node.props.iter().find(|&(k, v)| **k == *key && *v == *value ).is_some())
			.map(|node|
				node.id
			).collect()
    }

	/*******************/
	/****** Edges ******/
	/*******************/

	/// Add an Edge to the graph from the source NodeIndex to the target NodeIndex with a label
    pub fn connect_nodes(&mut self, origin:NodeIndex, destination:NodeIndex, label:&str) {
        if !self.nodes.contains_key(&origin) {
            panic!("Tried to connect node id that wasn't in the database: {}", origin)
        }

        if !self.nodes.contains_key(&destination) {
            panic!("Tried to connect node id that wasn't in the database: {}", destination)
        }
		let src_map = self.edges.entry(origin).or_insert(HashMap::new());
		match src_map.entry(destination) {
			Entry::Vacant(e) => {
				let mut s = HashSet::new();
				s.insert(label.to_string().into_boxed_str());
				e.insert(Edge { labels: s });
			},
			Entry::Occupied(mut e) => if !e.get().labels.contains(label) { e.get_mut().labels.insert(label.to_string().into_boxed_str()); } else {()}
		};

        if match self.reverse_edges.get_mut(&origin){
            Some(m) => {
                if m.contains(&destination) {
                    m.remove(&destination);
                    false
                }
                else {
                    true
                }
            },
            None => true
        } {
            if !self.reverse_edges.contains_key(&destination) {
                self.reverse_edges.insert(destination, HashSet::new());
            }
            self.reverse_edges.get_mut(&destination).unwrap().insert(origin);
        }
    }

	//Remove the connection between two nodes
	pub fn disconnect_nodes(&mut self, origin: NodeIndex, destination: NodeIndex, label: &str) {
		if !self.nodes.contains_key(&origin) {
            panic!("Tried to disconnect node id that wasn't in the database: {}", origin)
        }

        if !self.nodes.contains_key(&destination) {
            panic!("Tried to disconnect node id that wasn't in the database: {}", destination)
        }
		match self.edges.entry(origin) {
			Entry::Vacant(_) => panic!("Tried to disconnect nodes that were not connected: {}, {}", origin, destination),
			Entry::Occupied(mut map) => {
				match map.get_mut().entry(destination) {
					Entry::Vacant(_) => panic!("Tried to disconnect nodes that were not connected: {}, {}", origin, destination),
					Entry::Occupied(mut edge) => {
						if edge.get().labels.contains(label) {
							edge.get_mut().labels.remove(label);
							if edge.get().labels.is_empty() {
								edge.remove();
								match self.reverse_edges.entry(origin) {
									Entry::Vacant(re) => {
										let mut s = HashSet::new();
										s.insert(destination);
										re.insert(s);
									}
									Entry::Occupied(mut re) => {
										let mut hs = re.get_mut();
										if hs.contains(&destination) {
											hs.remove(&destination);
										} else {
											hs.insert(destination);
										}
									}
								}
							}
						} else {
							panic!("Nodes {} and {} were not connected by label {}", origin, destination, label);
						}
					}
				}
			}
		}
	}

	/// Check if two nodes are connected on the graph
    pub fn are_connected(&mut self, origin:NodeIndex, destination:NodeIndex) -> bool {
        if !self.nodes.contains_key(&origin) {
            panic!("Tried to check node id that wasn't in the database: {}", origin)
        }

        if !self.nodes.contains_key(&destination) {
            panic!("Tried to check node id that wasn't in the database: {}", destination)
        }
        match self.edges.get(&origin) {
            Some(m) => m.contains_key(&destination),
            None    => false
        }
	}

	pub fn edges_from(&self, source:NodeIndex) -> &HashMap<NodeIndex, Edge> {
		self.edges.get(&source).unwrap()
	}

    /// Get Edges with a given label
	pub fn edges_with_label(&self, label: &str) -> HashMap<&NodeIndex, HashMap<&NodeIndex, &Edge>> {
		let mut edges = HashMap::<&NodeIndex, HashMap<&NodeIndex, &Edge>>::new();
		for (src, map) in self.edges.iter() {
			let filtered: HashMap<&NodeIndex, &Edge> = map.iter().filter(|&(_idx, edge)| edge.labels.contains(label)).collect();
			if filtered.len() > 0 {
				edges.insert(src, filtered);
			}
		}
		edges
	}

	/// Get Nodes with a given label from a source NodeIndex
	pub fn edges_with_label_from(&self, source: NodeIndex, label: &str) -> Vec<NodeIndex> {
		if let Some(edge) = self.edges.get(&source) {
		edge.iter().filter_map(
			|(&idx, edge)|{
				if edge.labels.contains(label) {
					Some(idx)
				} else {
					None
				}}).collect()
			} else {
				vec![]
			}
	}

    pub fn match_paths<'a>(&'a self, matcher: &MatchingAutomaton<'a>) -> MatchResult {
        let mut closed_set = HashSet::new();
        let mut open_set = VecDeque::new();
        let mut parent_lookup = HashMap::new();
        let mut finished_nodes = HashSet::new();
        for node in self.nodes.values() {
            if let Some(next_state) = matcher.next_state_node(0, node) {
                parent_lookup.insert(node.id, Vec::new());
                if matcher.is_complete(next_state) {
                    finished_nodes.insert(node.id);
                } else {
                    open_set.push_back((node, next_state));

                }
            }
        }

        while let Some((node, state)) = open_set.pop_front() {
            if !closed_set.contains(&(node.id, state)) {
                closed_set.insert((node.id, state));
                let mut found_match = false;
                for (end_node_id, edge) in self.edges.get(&node.id).unwrap() {
                    if let Some(edge_state) = matcher.next_state_edge(state, &edge) {
                        if matcher.is_complete(edge_state) {
                            parent_lookup.entry(*end_node_id,).or_insert(Vec::new()).push((node.id, edge));
                            finished_nodes.insert(*end_node_id);
                            found_match = true;
                        } else {
                            let end_node = self.get_node(*end_node_id).unwrap();
                            if let Some(next_state) = matcher.next_state_node(edge_state, end_node) {
                                if matcher.is_complete(next_state) {
                                    found_match = true;
                                }
                                parent_lookup.entry(*end_node_id,).or_insert(Vec::new()).push((node.id, edge));
                                open_set.push_back((end_node, next_state))
                            }
                        }
                    }
                }
                if !found_match && matcher.is_complete(state) {
                    finished_nodes.insert(node.id);
                }
            }

        }
        MatchResult::new(parent_lookup, finished_nodes, self)
    }


	pub fn path_from(&self, source: NodeIndex, edge_predicate: &Fn(&str) -> bool, end_predicate: &Fn(NodeIndex) -> bool) -> Option<Path> {
		let mut open_set = LinkedList::new();
		let mut closed_set = HashSet::new();
		let root_path = Path {
			nodes: vec![source],
			edges: vec![]
		};
		open_set.push_back(root_path);
		closed_set.insert(source);
		while !open_set.is_empty() {
			let cur = open_set.pop_front().unwrap();
			let cur_node = cur.nodes.last().unwrap();
			closed_set.insert(*cur_node);
			if let Some(e) = self.edges.get(&cur_node) {
				for (child, edge) in e.iter() {
					for label in &edge.labels {
						if edge_predicate(&label) {
                            let mut child_path = cur.clone();
    						child_path.edges.push(label);
    						child_path.nodes.push(*child);
    						if end_predicate(*child) {
    							return Some(child_path);
    						} else {
    							if !closed_set.contains(child) {
    								open_set.push_back(child_path);
    							}
    						}
                            break;
						}
					}
				}
			}
		}
		None
	}

}

#[cfg(test)]
mod tests {
    use super::*;
    use super::matching::{NodeMatcher, node_with_id, MatchingAutomaton};
    use rustc_serialize::json::{Json, ToJson};

    macro_rules! prop_map (
    	{ $($key:expr => $value:expr),+ } => {
            {
                let mut dict = ::std::collections::HashMap::new();
                $(
                    dict.insert($key.to_string().into_boxed_str(),  PropVal::String($value.to_string().into_boxed_str()));
                )+
                dict
            }
         };
    );

    #[test]
    fn adding_nodes() {
        let mut g = Graph::new();
        let id1 = g.add_node();
        let id2 = g.add_node();
        {
            let n1 = g.get_node(id1).unwrap();
            assert!(n1.id == id1);
        }
        {
            let n2 = g.get_node_mut(id2).unwrap();
            assert!(n2.id == id2);
            n2.props.insert("hey".to_string().into_boxed_str(), PropVal::String("you".to_string().into_boxed_str()));
        }

    }

    #[test]
    fn connecting_nodes() {
        let mut g = Graph::new();
        let id1 = g.add_node();
        let id2 = g.add_node();
        let id3 = g.add_node();
        g.connect_nodes(id1, id2, "hello");
        assert!(g.are_connected(id1, id2));
        assert!(!g.are_connected(id2, id1));
        g.connect_nodes(id2, id1, "hi");
        g.connect_nodes(id2, id3, "hello");

        assert!(g.are_connected(id1, id2));
        assert!(g.are_connected(id2, id1));
        assert!(g.are_connected(id2, id3));
        assert!(!g.are_connected(id3, id2));

		g.connect_nodes(id1, id2, "second_label");
		println!("{:?}", g.edges_with_label_from(id1, "second_label"));
		assert!(g.edges_with_label_from(id1, "second_label") == vec![id2]);
		assert!(g.edges_with_label_from(id1, "hello") == vec![id2]);
    }

	#[test]
	fn disconnecting_nodes() {
		let mut g = Graph::new();
		let id1 = g.add_node();
        let id2 = g.add_node();
        let id3 = g.add_node();
		g.connect_nodes(id1, id2, "hello");
        g.connect_nodes(id2, id1, "hi");
        g.connect_nodes(id2, id3, "hello");
		g.disconnect_nodes(id1, id2, "hello");
		assert!(!g.are_connected(id1, id2));
		assert!(g.are_connected(id2, id1));
		g.remove_node(id1);
		assert!(1 == g.edges_from(id2).len());
		assert!(g.are_connected(id2, id3));
	}

    #[test]
    fn removing_nodes() {
        let mut g = Graph::new();
        let id1 = g.add_node();
        let id2 = g.add_node();
        let id3 = g.add_node();
        g.connect_nodes(id1, id2, "hello");
        assert!(g.are_connected(id1, id2));
        assert!(!g.are_connected(id2, id1));
        g.connect_nodes(id2, id1, "hi");
        g.connect_nodes(id2, id3, "what's new");

        assert!(g.are_connected(id1, id2));
        assert!(g.are_connected(id2, id1));
        assert!(g.are_connected(id2, id3));
        assert!(!g.are_connected(id3, id2));
        assert!(!g.are_connected(id3, id1));
        assert!(!g.are_connected(id1, id3));

        g.remove_node(id3);
        assert!(g.are_connected(id1, id2));
        assert!(g.are_connected(id2, id1));
        assert!(g.get_node(id3).is_none());
    }
	#[test]
	fn node_properties() {
		let mut g = Graph::new();
		let props_hash_map1 = prop_map! {
            "prop1" => "val1",
            "prop2" => "val2",
            "prop3" => "val3"
        };

        let props_hash_map2 = prop_map! {
            "prop2" => "val2",
            "prop3" => "val3"
        };
		let id1 = g.add_node_with_props(props_hash_map1);
		let id2 = g.add_node_with_props(props_hash_map2);

		let props_list = g.nodes_with_prop("prop2", &PropVal::String("val2".to_string().into_boxed_str()));
		assert!(props_list.len() == 2);
		assert!(props_list.contains(&id1));
		assert!(props_list.contains(&id2));

		let props_list2 = g.nodes_with_prop("prop1", &PropVal::String("val1".to_string().into_boxed_str()));
		assert!(props_list2.len() == 1);
		assert!(props_list2.contains(&id1));
	}

    #[test]
    fn json_io() {
        let mut g = Graph::new();
        let id1 = g.add_node();
        let id2 = g.add_node();
        let id3 = g.add_node();
        g.connect_nodes(id1, id2, "hello");
        g.connect_nodes(id2, id1, "hi");
        g.connect_nodes(id2, id3, "what's new");

        let json_string = g.to_json().to_string();
        let expected_string = r#"{"edges":{"1":{"2":{"labels":["hello"]}},"2":{"1":{"labels":["hi"]},"3":{"labels":["what's new"]}}},"nodes":{"1":{"id":1,"props":{}},"2":{"id":2,"props":{}},"3":{"id":3,"props":{}}}}"#;
        assert!(json_string == expected_string);
        let new_json = Json::from_str(expected_string).unwrap();
        let g2 = Graph::from_json(new_json);
        assert!(g.eq(&g2));
    }

	#[test]
	fn get_path() {
		let mut g = Graph::new();
        let id1 = g.add_node();
        let id2 = g.add_node();
        let id3 = g.add_node();
        let id4 = g.add_node();
        let id5 = g.add_node();
        let id6 = g.add_node();
        let id7 = g.add_node();
        let id8 = g.add_node();
        let id9 = g.add_node();
        let id10 = g.add_node();
        let id11 = g.add_node();
        let id12 = g.add_node();
        let id13 = g.add_node();
        let id14 = g.add_node();
		g.connect_nodes(id1, id2, "conn1");
		g.connect_nodes(id1, id3, "conn2");
		g.connect_nodes(id2, id4, "conn3");
		g.connect_nodes(id2, id5, "conn4");
		g.connect_nodes(id3, id6, "conn5");
		g.connect_nodes(id4, id7, "conn6");
		g.connect_nodes(id5, id7, "conn7");
		g.connect_nodes(id6, id5, "conn8");
		g.connect_nodes(id6, id12, "conn9");
		g.connect_nodes(id7, id9, "conn10");
		g.connect_nodes(id7, id10, "conn11");
		g.connect_nodes(id7, id11, "conn12");
		g.connect_nodes(id7, id13, "conn13");
		g.connect_nodes(id8, id11, "conn14");
		g.connect_nodes(id9, id14, "conn15");
		g.connect_nodes(id10, id14, "conn16");
		g.connect_nodes(id10, id13, "conn17");
		let result = g.path_from(id1, &|_| true, &|id| id == id14);
		match result {
			Some(r) => {
				assert!(6 == r.nodes.len());
				assert!(5 == r.edges.len());
			}
			None => {
				panic!("Could not find path");
			}
		};
		let result = g.path_from(id6, &|_| true, &|id| id == id13);
		match result {
			Some(r) => {
				assert!(r.nodes == vec![id6, id5, id7, id13]);
				assert!(r.edges == vec!["conn8", "conn7", "conn13"]);
			}
			None => {
				panic!("Could not find path");
			}
		};
		if g.path_from(id14, &|_| true, &|id| id == id4).is_some() {
			panic!("Found path when there should not be one")
		}

	}

    #[test]
    fn test_matching() {
        let g = Graph::read_from_file("data/langs.graph".to_string());
        let matcher = node_with_id(7).connected_by_label("influenced").to(NodeMatcher::Any);
        let result = g.match_paths(&MatchingAutomaton::from_path_matcher(&matcher));
        println!("{:?}", result.to_dag());
    }
}
