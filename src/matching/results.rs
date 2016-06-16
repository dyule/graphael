use std::collections::{HashMap, HashSet};
use super::automata::State;
use rustc_serialize::{Encoder, Encodable};
use ::{Node, NodeIndex, Edge, GraphDB, Graph, PropVal};
use std::hash::{Hash, Hasher};

pub struct MatchResult<'a> {
    parent_lookup: HashMap<NodeIndex, Vec<(NodeIndex, &'a Edge)>>,
    finished_nodes: HashSet<NodeIndex>,
    ref_graph: &'a GraphDB
}

#[derive(Clone, Debug, PartialEq)]
pub struct EdgeNode {
    edge: Edge,
    node: Node
}

#[derive(Clone, Debug, PartialEq)]
pub struct ResultRow {
    head: Node,
    tail: Vec<EdgeNode>
}

#[derive(Debug, PartialEq)]
pub struct ResultSet {
    rows: Vec<ResultRow>
}

#[derive(Debug)]
pub struct DAG{
    roots: HashSet<NodeIndex>,
    nodes: HashMap<NodeIndex, DAGNode>
}


#[derive(Debug)]
pub struct DAGNode {
    node: Node,
    connected_to: HashMap<NodeIndex, Edge>,
}

impl<'a> MatchResult<'a> {
    pub fn new(parent_lookup: HashMap<NodeIndex, Vec<(NodeIndex, &'a Edge)>>, finished_nodes: HashSet<State>, graph: &'a GraphDB) -> MatchResult<'a> {
        MatchResult {
            parent_lookup: parent_lookup,
            finished_nodes: finished_nodes,
            ref_graph: graph
        }
    }

    pub fn to_result_set(&self) -> ResultSet {
        let mut rows = Vec::new();
        for node in self.finished_nodes.iter() {
            rows.push(ResultRow{
                head: self.ref_graph.get_node(*node).unwrap().clone(),
                tail: Vec::new()
            });
            let index = rows.len() - 1;
            self.add_node_to_row(*node, &mut rows, index);
        }
        ResultSet {
            rows: self.reverse_rows(rows)
        }
    }

    fn add_node_to_row(&self, node: NodeIndex, rows: &mut Vec<ResultRow>, row_index: usize) {
        let mut first = true;
        for &(parent, edge) in self.parent_lookup.get(&node).unwrap() {
            let index = if first {
                first = false;
                row_index
            } else {
                let new_row = rows[row_index].clone();
                rows.push(new_row);
                rows.len() - 1
            };
            rows[index].tail.push(EdgeNode {
                edge: edge.clone(),
                node: self.ref_graph.get_node(parent).unwrap().clone()
            });
            self.add_node_to_row(parent, rows, index);
        }
    }

    fn reverse_rows(&self, rows: Vec<ResultRow>) -> Vec<ResultRow> {
        let mut new_rows = Vec::new();
        for row in rows {
            if row.tail.len() == 0 {
                new_rows.push(row)
            } else {
                let mut hanging_edge = None;
                let mut first = true;
                let mut new_row = ResultRow {
                    // XXX This is an extra copy that we don't technically need, but rust complains
                    // if we move the value
                    head: row.tail.get(row.tail.len() - 1).unwrap().node.clone(),
                    tail: Vec::new()
                };
                for edge_node in row.tail.into_iter().rev() {
                    if first {
                        first = false;
                    } else {
                        new_row.tail.push(
                            EdgeNode {
                                node: edge_node.node,
                                edge: hanging_edge.unwrap()
                            }
                        );
                    }
                    hanging_edge = Some(edge_node.edge);

                }
                new_row.tail.push(EdgeNode {
                    node: row.head,
                    edge: hanging_edge.unwrap()
                });
                new_rows.push(new_row);
            }
        }
        new_rows

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

    fn add_node_rec(&self, node: NodeIndex, child: Option<(NodeIndex, &'a Edge)>, nodes: &mut HashMap<NodeIndex, DAGNode>, roots: &mut HashSet<NodeIndex>) {
        {
            let mut dag_node = nodes.entry(node).or_insert(DAGNode{node: self.ref_graph.get_node(node).unwrap().clone(), connected_to: HashMap::new()});
            if let Some(child) = child {
                if !dag_node.connected_to.contains_key(&child.0) {
                    dag_node.connected_to.insert(child.0, child.1.clone());
                } else {
                    return
                }
            }
        }
        let lookup = self.parent_lookup.get(&node).unwrap();
        if lookup.is_empty() {
            roots.insert(node);
        } else {
            for &(parent, edge) in lookup {
                self.add_node_rec(parent, Some((node, edge)), nodes, roots)
            }
        }
    }
}

struct Link {
    source: NodeIndex,
    target: NodeIndex
}

impl Encodable for DAG {
    fn encode<E:Encoder>(&self, encoder: &mut E) -> Result<(), E::Error> {
        let mut node_array = Vec::new();
        let mut link_array = Vec::new();
        let mut id_lookup = HashMap::new();

        for (index, (id, node)) in self.nodes.iter().enumerate() {
            node_array.push(&node.node);
            id_lookup.insert(id, index);
        }
        for (id, node) in self.nodes.iter() {
            let id = id_lookup.get(id).unwrap();
            for (target, _) in node.connected_to.iter() {
                link_array.push(Link{source: id.clone(), target: id_lookup[target].clone()})
            }
        }
        encoder.emit_struct("root", 2, |encoder| {
            try!(encoder.emit_struct_field("nodes", 0, |encoder| {
                node_array.encode(encoder)
            }));
            encoder.emit_struct_field("links", 1, |encoder| {
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

impl PartialEq<DAGNode> for DAGNode {
    fn eq(&self, other: &DAGNode) -> bool {
        self.node == other.node
    }
}


impl Hash for DAGNode {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.node.hash(state)
    }
}

impl Graph for DAG {
    fn get_node(&self, node_id: NodeIndex) -> Option<&Node> {
        match self.nodes.get(&node_id) {
            Some(ref dag_node) => Some(&dag_node.node),
            None => None
        }
    }


    /// Get Nodes with a key-value pair
    fn nodes_with_prop(&self, key: &str, value: &PropVal) -> Vec<NodeIndex> {
        self.nodes.values().filter(|node|
            node.node.props.iter().find(|&(k, v)| **k == *key && *v == *value ).is_some())
            .map(|node|
                node.node.id
            ).collect()
    }

    fn are_connected(&mut self, origin: NodeIndex, destination: NodeIndex) -> bool {
        match self.nodes.get(&origin) {
            Some(ref node) => node.connected_to.contains_key(&destination),
            None => false
        }
    }

    fn edges_from(&self, source: NodeIndex) -> &HashMap<NodeIndex, Edge> {
        &self.nodes.get(&source).unwrap().connected_to
    }

    fn edges_with_label(&self, label: &str) -> HashSet<(NodeIndex, NodeIndex)> {
        self.nodes.iter().flat_map(|(src, node)|
            node.connected_to.iter().filter(|&(_idx, edge)|
                edge.labels.contains(label)
            ).map(move |(idx, _edge)| (*src, *idx))
        ).collect()
    }

    fn edges_with_label_from(&self, source: NodeIndex, label: &str) -> Vec<NodeIndex> {
        if let Some(node) = self.nodes.get(&source) {
		node.connected_to.iter().filter_map(
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
}

#[cfg(test)]
mod test {
    use ::{GraphDB, Edge, NodeIndex, Graph};
    use super::{ResultRow, EdgeNode};

    #[test]
    fn result_set_rust_influenced_by() {
        fn check_row(rows: &Vec<ResultRow>, g: &GraphDB, end_node: NodeIndex) {
            let my_row = ResultRow {
                head: g.get_node(112).unwrap().clone(),
                tail: vec![EdgeNode{
                    node: g.get_node(end_node).unwrap().clone(),
                    edge: Edge {
                        labels: vec!["influencedBy".to_string().into_boxed_str()].into_iter().collect()
                    },
                }]
            };
            for row in rows {
                if &my_row == row {
                    return
                }
            }
            panic!("Could not find row {:?} in rows {:?}", my_row, rows);

        }
        let g = GraphDB::read_from_file("data/langs.graph").unwrap();
        let results = g.match_paths("(112) -influencedBy> ()").unwrap().to_result_set();
        assert_eq!(5, results.rows.len());
        check_row(&results.rows, &g, 212);
        check_row(&results.rows, &g, 116);
        check_row(&results.rows, &g, 143);
        check_row(&results.rows, &g, 245);
        check_row(&results.rows, &g, 179);
    }
}
