mod indexing;
mod serde;

use std::collections::{HashMap, HashSet,  VecDeque};
use std::collections::hash_map::Entry;
use rustc_serialize::Decodable;
use rustc_serialize::json::{self, Json, ToJson};
use std::fs::File;
use std::io::{self, Read, Write};
use std::string::ToString;
use matching::*;
use std::hash::{Hash, Hasher};
use queries::ParseError;
use ::{Node, Graph, GraphDB, Edge, PropVal, NodeIndex, IntoAutomata, GraphInternal};
use std::sync::{Arc, RwLock, Weak};
use std::fmt;

#[derive(Debug)]
pub enum PropIndexEntry {
    Unindexed(HashSet<NodeIndex>),
    Indexed(HashMap<PropVal, HashSet<NodeIndex>>)
}

/***********************/
/*** Implementations ***/
/***********************/

impl PartialEq for GraphDB {
    fn eq(&self, other:&Self) -> bool {
        return self.nodes.eq(&other.nodes) && self.edges.eq(&other.edges)
    }
}

impl PartialEq<Node> for Node {
    fn eq(&self, other: &Node) -> bool {
        self.id == other.id
    }
}


impl Hash for Node {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.id.hash(state)
    }
}

impl Hash for PropVal {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        match self {
            &PropVal::Int(i) => i.hash(state),
            &PropVal::String(ref s) => s.hash(state)
        }
    }
}

impl fmt::Display for PropVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &PropVal::Int(i) => i.fmt(f),
            &PropVal::String(ref s) => s.fmt(f)
        }
    }
}

impl Node {

    /// Sets the value of a property.
    /// Updates any indices for the property in question
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
    pub fn set_prop(&mut self, prop: &str, value: PropVal) -> Option<PropVal> {
        let prop = prop.to_string().into_boxed_str();
        let graph = Weak::upgrade(&self.graph.clone()).unwrap();
        graph.write().unwrap().update_prop_index(self, &prop, &value);
        self.props.insert(prop, value)
    }

    /// Unsets a previously set property
    /// Updates any indices for the value in question
    /// # Example
    ///
    /// ```
    /// # use graphael::{GraphDB, PropVal};
    /// let mut graph = GraphDB::new();
    /// let id = graph.add_node();
    /// let node = graph.get_node_mut(id).unwrap();
    /// node.set_prop("this_prop", PropVal::Int(5));
    /// node.set_prop("another_prop", PropVal::Int(94));
    /// node.unset_prop("this_prop");
    /// assert!(node.get_prop("this_prop").is_none())
    /// ```
    pub fn unset_prop(&mut self, prop: &str) -> Option<PropVal> {
        let prop = prop.to_string().into_boxed_str();
        let graph = Weak::upgrade(&self.graph.clone()).unwrap();
        let mut  graph = graph.write().unwrap();
        let value = self.props.remove(&prop);
        if let Some(ref value) = value {
            graph.remove_prop_index(self, &prop, &value);
        }
        value
    }


    /// Gets the value of a property.
    ///
    /// # Example
    ///
    /// ```
    /// # use graphael::{GraphDB, PropVal};
    /// let mut graph = GraphDB::new();
    /// let id = graph.add_node();
    /// let node = graph.get_node_mut(id).unwrap();
    /// node.set_prop("this_prop", PropVal::Int(5));
    /// assert_eq!(node.get_prop("this_prop").unwrap(), &PropVal::Int(5));
    /// ```
    pub fn get_prop(&self, prop: &str) -> Option<&PropVal> {
        let prop = prop.to_string().into_boxed_str();
        self.props.get(&prop)
    }
}

impl GraphInternal {
    fn update_prop_index(&mut self, node: &Node, prop: &Box<str>, value: &PropVal) {
        match self.prop_index.entry(prop.clone()).or_insert(PropIndexEntry::Unindexed(HashSet::new())) {
            &mut PropIndexEntry::Unindexed(ref mut indexes) => {
                indexes.insert(node.id);
            },
            &mut PropIndexEntry::Indexed(ref mut mapping) => {
                if let Some(ref mut indexes) = mapping.get_mut(value) {
                    indexes.remove(&node.id);
                }
                let mut indexes = mapping.entry(value.clone()).or_insert(HashSet::new());
                indexes.insert(node.id);

            }
        }
    }

    fn remove_prop_index(&mut self, node: &Node, prop: &Box<str>, value: &PropVal) {
        if let Some(prop_index) = self.prop_index.get_mut(prop) {
            match prop_index {
                &mut PropIndexEntry::Unindexed(ref mut indexes) => {
                    indexes.insert(node.id);
                },
                &mut PropIndexEntry::Indexed(ref mut mapping) => {
                    if let Some(ref mut indexes) = mapping.get_mut(value) {
                        indexes.remove(&node.id);
                    }
                }
            }
        }
    }

    fn update_edge_index(&mut self, src: NodeIndex, dst: NodeIndex, label: &Box<str>) {
        let mut index = self.label_index.entry(label.clone()).or_insert(HashSet::new());
        index.insert((src, dst));
    }

    fn remove_edge_from_index(&mut self, src: NodeIndex, dst: NodeIndex, label: &str) {
        if let Some(index) = self.label_index.get_mut(label) {
            index.remove(&(src, dst));
        }
    }
}

/*** GraphDB ***/

impl GraphDB {

	/// Creates a new graph with no nodes or edges
    #[inline]
    pub fn new() -> GraphDB {
        GraphDB {
            max_node_id: 0,
            nodes: HashMap::new(),
            edges: HashMap::new(),
            reverse_edges: HashMap::new(),
            internal: Arc::new(RwLock::new(GraphInternal{
                prop_index: HashMap::new(),
                label_index: HashMap::new()
            }))
        }
    }

	/// Encode and decode from file

	pub fn from_json(json: Json) -> GraphDB {
		let mut decoder = json::Decoder::new(json);
		match Decodable::decode(&mut decoder) {
			Ok(x) => x,
			Err(e) => panic!("Could not decode to graph: {}", e)
		}
	}

	pub fn read_from_file(name: &str) -> io::Result<GraphDB> {
		let mut contents = String::new();
		let mut file:File = try!(File::open(name));

		try!(file.read_to_string(&mut contents));
		let jsonstring = match Json::from_str(&contents) {
			Ok(jstr) => jstr,
			Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e))
		};

		Ok(GraphDB::from_json(jsonstring))
	}

	pub fn write_to_file(&self, name: &'static str) -> io::Result<usize> {
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

	/// Adds a Node to the GraphDB
    pub fn add_node(&mut self) -> NodeIndex {
        let idx = self.get_node_next_id();
        let node:Node = Node {
            graph: Arc::downgrade(&self.internal.clone()),
            id: idx,
            props: HashMap::new()
        };
        self.nodes.insert(idx, node);
		idx
    }

	/// Adds a Node to the GraphDB with the specified properties
	pub fn add_node_with_props(&mut self, props: HashMap<Box<str>, PropVal>) -> NodeIndex {
		//let id = self.add_node();
        let id = self.get_node_next_id();
        let node:Node = Node {
            graph: Arc::downgrade(&self.internal.clone()),
            id: id,
            props: props
        };
        self.nodes.insert(id, node);
		id
	}

	// Removes a Node from the GraphDB given a NodeIndex
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
        let label = label.to_string().into_boxed_str();
        self.internal.write().unwrap().update_edge_index(origin, destination, &label);
		match src_map.entry(destination) {
			Entry::Vacant(e) => {
				let mut s = HashSet::new();
				s.insert(label);
				e.insert(Edge { labels: s });
			},
			Entry::Occupied(mut e) => {
                e.get_mut().labels.insert(label);
            }
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
                            self.internal.write().unwrap().remove_edge_from_index(origin, destination, label);
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

    pub fn match_paths<'a, I: IntoAutomata<'a>>(&'a self, matcher: I) -> Result<MatchResult, ParseError> {
        let matcher = try!(matcher.into_automata());
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
                    if let Some(edge_state) = matcher.next_state_edge(state, &edge.labels) {
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
        Ok(MatchResult::new(parent_lookup, finished_nodes, self))
    }

}

impl Graph for GraphDB {

    fn get_node(&self, node_id:NodeIndex) -> Option<&Node> {
        self.nodes.get(&node_id)
    }


    /// Get Nodes with a key-value pair
    fn nodes_with_prop(&self, key: &str, value: &PropVal) -> Vec<NodeIndex> {
        self.nodes.values().filter(|node|
            node.props.iter().find(|&(k, v)| **k == *key && *v == *value ).is_some())
			.map(|node|
				node.id
			).collect()
    }

	/// Check if two nodes are connected on the graph
    fn are_connected(&mut self, origin:NodeIndex, destination:NodeIndex) -> bool {
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

	fn edges_from(&self, source:NodeIndex) -> &HashMap<NodeIndex, Edge> {
		self.edges.get(&source).unwrap()
	}

    /// Get Edges with a given label
	fn edges_with_label<'a>(&'a self, label: &str) -> HashSet<(NodeIndex, NodeIndex)> {
		self.internal.read().unwrap().label_index.get(label).map(|set| set.clone()).unwrap_or(HashSet::new())
	}

	/// Get Nodes with a given label from a source NodeIndex
	fn edges_with_label_from(&self, source: NodeIndex, label: &str) -> Vec<NodeIndex> {
		self.edges_with_label(label).iter().filter_map(|&(src, dst)|
            if src == source {
                Some(dst)
            } else {
                None
            }
        ).collect()
	}

}

#[cfg(test)]

mod tests {
    use matching::{NodeMatcher, node_with_id, MatchingAutomaton};
    use rustc_serialize::json::{Json, ToJson};
    use super::super::*;

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
        let mut g = GraphDB::new();
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
        let mut g = GraphDB::new();
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
		assert!(g.edges_with_label_from(id1, "second_label") == vec![id2]);
		assert!(g.edges_with_label_from(id1, "hello") == vec![id2]);
    }

	#[test]
	fn disconnecting_nodes() {
		let mut g = GraphDB::new();
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
		assert_eq!(g.edges_from(id2).len(), 1);
		assert!(g.are_connected(id2, id3));
        assert_eq!(g.edges_with_label_from(id2, "hello"), vec![id3]);
	}

    #[test]
    fn removing_nodes() {
        let mut g = GraphDB::new();
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
		let mut g = GraphDB::new();
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
        let mut g = GraphDB::new();
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
        let g2 = GraphDB::from_json(new_json);
        assert!(g.eq(&g2));
    }

    #[test]
    fn test_matching() {
        let g = GraphDB::read_from_file("data/langs.graph").unwrap();
        let matcher = node_with_id(7).connected_by_label("influenced").to(NodeMatcher::Any);
        let result = g.match_paths(MatchingAutomaton::from_path_matcher(&matcher)).unwrap();
        println!("{:?}", result.to_dag());
    }
}
