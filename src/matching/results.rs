use std::collections::{HashMap, HashSet};
use super::automata::State;
use rustc_serialize::{Encoder, Encodable};
use ::{Node, NodeIndex, Edge, GraphDB, Graph};
use std::hash::{Hash, Hasher};

pub struct MatchResult<'a> {
    parent_lookup: HashMap<NodeIndex, Vec<(NodeIndex, &'a Edge)>>,
    finished_nodes: HashSet<NodeIndex>,
    ref_graph: &'a GraphDB
}

pub struct EdgeNode {
    edge: Edge,
    node: Node
}

pub struct ResultRow {
    head: Node
}

pub struct ResultSet {
    rows: Vec<ResultRow>
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

impl<'a> MatchResult<'a> {
    pub fn new(parent_lookup: HashMap<NodeIndex, Vec<(NodeIndex, &'a Edge)>>, finished_nodes: HashSet<State>, graph: &'a GraphDB) -> MatchResult<'a> {
        MatchResult {
            parent_lookup: parent_lookup,
            finished_nodes: finished_nodes,
            ref_graph: graph
        }
    }

    pub fn to_dag(&self) -> DAG {
        let mut nodes = HashMap::new();
        let mut roots = HashSet::new();
        for node in self.finished_nodes.iter() {
            self.add_node_rec(*node, None, &mut nodes, &mut roots);
        }
        DAG {
            nodes: nodes,
            roots: roots
        }
    }

    fn add_node_rec(&self, node: NodeIndex, child: Option<NodeIndex>, nodes: &mut HashMap<NodeIndex, DAGNode<'a>>, roots: &mut HashSet<NodeIndex>) {
        {
            let mut dag_node = nodes.entry(node).or_insert(DAGNode{node: self.ref_graph.get_node(node).unwrap(), connected_to: Vec::new()});
            if let Some(child) = child {
                if !dag_node.connected_to.contains(&child) {
                    dag_node.connected_to.push(child);
                } else {
                    return
                }
            }
        }
        let lookup = self.parent_lookup.get(&node).unwrap();
        if lookup.is_empty() {
            roots.insert(node);
        } else {
            for &(parent, _) in lookup {
                self.add_node_rec(parent, Some(node), nodes, roots)
            }
        }
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

impl<'a> PartialEq<DAGNode<'a>> for DAGNode<'a> {
    fn eq(&self, other: &DAGNode) -> bool {
        self.node == other.node
    }
}

impl<'a> Eq for DAGNode<'a> {}

impl <'a> Hash for DAGNode<'a> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.node.hash(state)
    }
}
