use ::{NodeIndex, Node, Edge};
use super::PropIndexEntry;
use std::collections::{HashMap, HashSet};

pub fn create_prop_index(nodes: &HashMap<NodeIndex, Node>) -> HashMap<Box<str>, PropIndexEntry> {
    let mut index = HashMap::new();
    for node in nodes.values() {
        for (k, _) in node.props.iter() {
            let mut entries = index.entry(k.clone()).or_insert(PropIndexEntry::Unindexed(HashSet::new()));
            if let &mut PropIndexEntry::Unindexed(ref mut entries) = entries {
                entries.insert(node.id);
            }
        }
    }
    index
}

pub fn create_edge_index(edges: &HashMap<NodeIndex, HashMap<NodeIndex, Edge>>) -> HashMap<Box<str>, HashSet<(NodeIndex, NodeIndex)>> {
    let mut index = HashMap::new();
    for (src, dst_map) in edges.iter() {
        for (dst, edge) in dst_map.iter() {
            for label in edge.labels.iter() {
                let mut entries = index.entry(label.clone()).or_insert(HashSet::new());
                entries.insert((*src, *dst));
            }

        }
    }
    index
}
