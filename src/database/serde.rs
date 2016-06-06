/// Handles SERialization and DEserialization of the graph, using rustc_serialize.
/// Should, at some point, be replaced by an implementation using serde

use rustc_serialize::{Decoder, Decodable, Encoder, Encodable};
use rustc_serialize::json::{self, Json, ToJson};
use ::{GraphDB, Node, Edge, PropVal, GraphInternal, NodeIndex};
use std::collections::{HashMap, HashSet};
use database::indexing::{create_prop_index, create_edge_index};
use std::sync::{Arc, RwLock, Weak};

impl <'a> ToJson for GraphDB {
	fn to_json(&self) -> Json {
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

impl Encodable for GraphDB {
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
                graph: Weak::new(),
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

impl Decodable for GraphDB {

    fn decode<D:Decoder>(decoder: &mut D) -> Result<Self, D::Error> {
        let internal =  Arc::new(RwLock::new(
                GraphInternal{
                    prop_index: HashMap::new(),
                    label_index: HashMap::new()
                }
            ));

        decoder.read_struct("root", 2, |decoder| {
            let mut max_node_id = 0;
            let nodes = try!(decoder.read_struct_field("nodes", 1, |decoder| {
                decoder.read_map(|decoder, len| {
                    let mut nodes = HashMap::new();
                    for idx in 0..len {
                        let node_index = try!(decoder.read_map_elt_key(idx, |decoder| decoder.read_usize()));
                        let mut node:Node = try!(decoder.read_map_elt_val(idx, |decoder| Decodable::decode(decoder)));
                        if node_index > max_node_id {
                            max_node_id = node_index + 1;
                        }
                        node.graph = Arc::downgrade(&internal.clone());
                        nodes.insert(node_index, node);
                    }
                    Ok(nodes)
                })

        }));
        let prop_index = create_prop_index(&nodes);

        let mut g = GraphDB{
                max_node_id: max_node_id,
                nodes: nodes,
                edges: HashMap::new(),
                reverse_edges: HashMap::new(),
                internal: internal
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
            let edge_index = create_edge_index(&g.edges);
            {
                let mut internal = g.internal.write().unwrap();
                internal.prop_index = prop_index;
                internal.label_index = edge_index;
            }
            Ok(g)
        })

    }
}
