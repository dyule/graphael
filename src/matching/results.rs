use std::collections::{HashMap, HashSet};
use super::automata::State;
use ::{NodeIndex, Edge, DAG, DAGNode};

pub struct MatchResult<'a> {
    parent_lookup: HashMap<NodeIndex, Vec<(NodeIndex, &'a Edge)>>,
    finished_nodes: HashSet<NodeIndex>
}

impl<'a> MatchResult<'a> {
    pub fn new(parent_lookup: HashMap<NodeIndex, Vec<(NodeIndex, &'a Edge)>>, finished_nodes: HashSet<State>) -> MatchResult<'a> {
        MatchResult {
            parent_lookup: parent_lookup,
            finished_nodes: finished_nodes
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

    fn add_node_rec(&self, node: NodeIndex, child: Option<NodeIndex>, nodes: &mut HashMap<NodeIndex, DAGNode>, roots: &mut HashSet<NodeIndex>) {
        {
            let mut dag_node = nodes.entry(node).or_insert(DAGNode{id: node, connected_to: Vec::new()});
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
