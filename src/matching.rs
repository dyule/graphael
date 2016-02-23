use ::{NodeIndex, Node, PropVal};
use std::ops::Deref;
use std::cell::Cell;
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, PartialEq)]
pub enum NodeMatcher {
    Any,
    Prop(Box<str>),
    PropVal(Box<str>, Box<PropVal>),
    Id(NodeIndex),
    And(Box<NodeMatcher>, Box<NodeMatcher>),
    Or(Box<NodeMatcher>, Box<NodeMatcher>)
}

static ANY: NodeMatcher =  NodeMatcher::Any;
pub enum PathMatcher {
    Labels(Vec<Box<str>>, NodeMatcher),
    Repeat(Option<u32>, Option<u32>, Box<PathMatcher>),
    ThenLabels(Vec<Box<str>>, Box<PathMatcher>),
    To(NodeMatcher, Box<PathMatcher>),
    Or(Box<PathMatcher>, Box<PathMatcher>)
}

type State = usize;

#[derive(PartialEq)]
pub enum Transition<'a> {
    NodeTransition(&'a NodeMatcher, State),
    EdgeTransition(&'a Box<str>, State),
    EpsilonTransition(State), // allows moving to the next state without consuming any input
}

struct StateGenerator {
    max_state: Cell<State>
}

#[derive(PartialEq)]
pub struct MatchingAutomaton<'a> {
    transitions: Vec<Vec<Transition<'a>>>
}

/// Find all of the states that can be gotten to from the starting state and that end on the final state
/// Performs a breadth first tranversal of the graph, starting at starting_state and ending on final_state
fn gather_states<'a>(starting_state: State, final_state: State, transitions: &mut Vec<Vec<Transition<'a>>>) -> HashSet<State> {
    let mut found_states = HashSet::new();
    let mut open_set = VecDeque::new();
    open_set.push_back(starting_state);
    found_states.insert(final_state);
    while !open_set.is_empty() {
        let current = open_set.pop_front().unwrap();
        if !found_states.contains(&current) {
            found_states.insert(current);
            for transition in transitions.get(current).unwrap() {
                let child = match transition {
                    &Transition::NodeTransition(_, child) => child,
                    &Transition::EdgeTransition(_, child) => child,
                    &Transition::EpsilonTransition(child) => child
                };
                if !found_states.contains(&child) {
                    open_set.push_back(child);
                }
            }
        }
    }
    found_states
}

impl<'a> MatchingAutomaton<'a> {
    pub fn from_path_matcher(matcher: &'a PathMatcher) -> MatchingAutomaton<'a> {
        let mut transitions = vec![Vec::new()];
        let factory = StateGenerator::new();
        MatchingAutomaton::process_path_matcher(matcher, &factory, 0, 0, &mut transitions);
        MatchingAutomaton{transitions:transitions}
    }

    fn process_path_matcher(matcher: &'a PathMatcher, factory: &StateGenerator, starting_state: State, final_state: State, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        match *matcher {
            PathMatcher::Labels(ref labels, ref node) => {
                MatchingAutomaton::from_node_and_labels(factory, starting_state, node, labels, transitions)
            },
            PathMatcher::Repeat(miniumum, maximum, ref child_matcher) => {
                let tail_state = MatchingAutomaton::process_path_matcher(child_matcher, factory, starting_state, final_state, transitions);
                MatchingAutomaton::repeat(factory, starting_state, tail_state, miniumum, maximum, transitions)
            },
            PathMatcher::ThenLabels(ref labels, ref child_matcher) => {
                let tail_state = MatchingAutomaton::process_path_matcher(child_matcher, factory, starting_state, final_state, transitions);
                MatchingAutomaton::from_labels(factory, tail_state, labels, transitions)
            },
             _ => {starting_state}
        }
    }

    fn from_node_and_labels(factory: &StateGenerator, starting_state: State, node: &'a NodeMatcher, labels: &'a Vec<Box<str>>, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        let node_state = factory.next_state();
        let final_state = factory.next_state();
        transitions[starting_state].push(Transition::NodeTransition(node, node_state));
        let label_transitions = labels.iter().map(|label| Transition::EdgeTransition(label, final_state)).collect();
        transitions.push(label_transitions);
        transitions.push(Vec::new());
        final_state
    }

    fn from_labels(factory: &StateGenerator, starting_state: State, labels: &'a Vec<Box<str>>, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        let node_state = factory.next_state();
        let final_state = factory.next_state();
        transitions[starting_state].push(Transition::NodeTransition(&ANY, node_state));
        let label_transitions = labels.iter().map(|label| Transition::EdgeTransition(label, final_state)).collect();
        transitions.push(label_transitions);
        transitions.push(Vec::new());
        final_state
    }

    fn repeat(factory: &StateGenerator, starting_state: State, final_state: State, minimum: Option<u32>, maximum: Option<u32>, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        let the_states = gather_states(starting_state, final_state, transitions);
        let minimum = match minimum {
            Some(m) => m,
            None => 0
        };
        let mut tail_state = final_state;
        let mut last_start = starting_state;
        for _ in 1..minimum {
            let new_states = MatchingAutomaton::duplicate_states(factory, &the_states, &starting_state, &tail_state, transitions);
            last_start = tail_state;
            tail_state = new_states.get(&final_state).unwrap().clone();
        }
        if let Some(maximum) = maximum {
            let mut skippable_starts = Vec::new();
            for _ in minimum..maximum {
                let new_states = MatchingAutomaton::duplicate_states(factory, &the_states, &starting_state, &tail_state, transitions);
                skippable_starts.push(tail_state);
                tail_state = new_states.get(&final_state).unwrap().clone();
            }
            for start in skippable_starts {
                transitions[start].push(Transition::EpsilonTransition(tail_state))
            }

        } else {
            transitions[tail_state].push(Transition::EpsilonTransition(last_start))
        }
        if minimum == 0 {
            transitions[starting_state].push(Transition::EpsilonTransition(tail_state));
        }
        tail_state
    }

    fn duplicate_states(factory: &StateGenerator, states: &HashSet<State>, starting_state: &State, final_state: &State, transitions: &mut Vec<Vec<Transition<'a>>>) -> HashMap<State, State> {
        let mut state_lookup = HashMap::new();

        // Create a new state for each existing state
        for state in states.iter() {
            // We match the starting state of the new states to the final state of the old states
            if state == starting_state {
                state_lookup.insert(*state, *final_state);
            } else {
                let new_state = factory.next_state();
                state_lookup.insert(*state, new_state);
            }

        }

        // Now that we've created the states, we iterate through the old states in the same
        // order as above, and copy the transitions
        for state in states.iter() {
            if state != final_state {
                let new_transitions = transitions[*state].iter().map(|transition| {
                    match transition {
                        &Transition::NodeTransition(node, ref new_state) => {
                            Transition::NodeTransition(node, *state_lookup.get(new_state).unwrap())
                        },
                        &Transition::EdgeTransition(edge, ref new_state) => {
                            Transition::EdgeTransition(edge, *state_lookup.get(new_state).unwrap())
                        },
                        &Transition::EpsilonTransition(ref new_state) => {
                            Transition::EpsilonTransition(*state_lookup.get(new_state).unwrap())
                        }
                    }
                }).collect();
                if state == starting_state {
                    transitions[*final_state] = new_transitions;
                } else {
                    transitions.push(new_transitions);
                }
            }
        }
        state_lookup
    }

}

impl StateGenerator {
    fn new() -> StateGenerator {
        StateGenerator{
            max_state: Cell::new(0)
        }
    }

    fn next_state(&self) -> State {
        self.max_state.set(self.max_state.get() + 1);
        self.max_state.get()
    }
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
    use super::*;
    use ::{PropVal, Graph};
    static ANY: NodeMatcher =  NodeMatcher::Any;
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

    #[test]
    fn test_node_label_match() {
        let path_matcher = node_with_id(6).connected_by_label("label1");
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        let expected_node = node_with_id(6);
        let expected_label = "label1".to_string().into_boxed_str();
        let expected_transitions = vec![
            vec![Transition::NodeTransition(&expected_node, 1)],
            vec![Transition::EdgeTransition(&expected_label, 2)],
            vec![]
        ];
        assert!(MatchingAutomaton{
            transitions: expected_transitions
        } == automaton)
    }

    #[test]
    fn test_node_multiple_label_match() {
        let expected_label1 = "label1".to_string().into_boxed_str();
        let expected_label2 = "label2".to_string().into_boxed_str();
        let expected_label3 = "label3".to_string().into_boxed_str();
        let path_matcher = node_with_id(6).connected_by_one_of(vec![expected_label1.clone(), expected_label2.clone(), expected_label3.clone()]);
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        let expected_node = node_with_id(6);

        let expected_transitions = vec![
            vec![Transition::NodeTransition(&expected_node, 1)],
            vec![Transition::EdgeTransition(&expected_label1, 2), Transition::EdgeTransition(&expected_label2, 2), Transition::EdgeTransition(&expected_label3, 2)],
            vec![]
        ];
        assert!(MatchingAutomaton{
            transitions: expected_transitions
        } == automaton)
    }

    #[test]
    fn test_node_then_labels() {
        let expected_label1 = "label1".to_string().into_boxed_str();
        let expected_label2 = "label2".to_string().into_boxed_str();
        let path_matcher = node_with_id(6).connected_by_label("label1").then_label("label2");
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        let expected_node = node_with_id(6);

        let expected_transitions = vec![
            vec![Transition::NodeTransition(&expected_node, 1)],
            vec![Transition::EdgeTransition(&expected_label1, 2)],
            vec![Transition::NodeTransition(&ANY, 3)],
            vec![Transition::EdgeTransition(&expected_label2, 4)],
            vec![]
        ];
        assert!(MatchingAutomaton{
            transitions: expected_transitions
        } == automaton)
    }

    fn test_node_then_labels_repeat() {
        let expected_label1 = "label1".to_string().into_boxed_str();
        let expected_label2 = "label2".to_string().into_boxed_str();
        let path_matcher = node_with_id(6).connected_by_label("label1").then_label("label2").repeat(Some(2), None);
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        let expected_node = node_with_id(6);

        let expected_transitions = vec![
            vec![Transition::NodeTransition(&expected_node, 1)],
            vec![Transition::EdgeTransition(&expected_label1, 2)],
            vec![Transition::NodeTransition(&ANY, 3)],
            vec![Transition::EdgeTransition(&expected_label2, 4)],
            vec![Transition::NodeTransition(&expected_node, 5)],
            vec![Transition::EdgeTransition(&expected_label1, 6)],
            vec![Transition::NodeTransition(&ANY, 7)],
            vec![Transition::EdgeTransition(&expected_label2, 8)],
            vec![]
        ];
        assert!(MatchingAutomaton{
            transitions: expected_transitions
        } == automaton)
    }
}
