use ::{NodeIndex, Node, PropVal, Edge};
use std::ops::Deref;
use std::cell::Cell;
use std::fmt;
use std::cmp;
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
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

pub type State = usize;

#[derive(PartialEq, Debug, Eq, Ord, Clone)]
enum Transition<'a> {
    NodeTransition(&'a NodeMatcher, State),
    EdgeTransition(&'a Box<str>, State),
    EpsilonTransition(State), // allows moving to the next state without consuming any input
}

struct StateGenerator {
    max_state: Cell<State>
}

#[derive(PartialEq)]
/// An automata that can match paths in a graph.
pub struct MatchingAutomaton<'a> {
    transitions: Vec<Vec<Transition<'a>>>
}

impl<'a> fmt::Debug for MatchingAutomaton<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{{");
        for (state, transition) in self.transitions.iter().enumerate() {
            writeln!(f, "  {}: {:?}", state, transition);
        }
        writeln!(f, "}}")
    }
}

impl <'a> cmp::PartialOrd<Transition<'a>> for Transition<'a> {
    fn partial_cmp(&self, other: &Transition<'a>) -> Option<cmp::Ordering> {
        match *self {
            Transition::NodeTransition(ref node, _) => {
                match *other {
                    Transition::NodeTransition(ref other_node, _) => node.partial_cmp(other_node),
                    Transition::EdgeTransition(_, _) => Some(cmp::Ordering::Less),
                    Transition::EpsilonTransition(_) => Some(cmp::Ordering::Less)
                }
            },
            Transition::EdgeTransition(ref edge, _) => {
                match *other {
                    Transition::NodeTransition(_, _) => Some(cmp::Ordering::Greater),
                    Transition::EdgeTransition(ref other_edge, _) => edge.partial_cmp(other_edge),
                    Transition::EpsilonTransition(_) => Some(cmp::Ordering::Less)
                }
            },
            Transition::EpsilonTransition(_) => {
                match *other {
                    Transition::NodeTransition(_, _) => Some(cmp::Ordering::Greater),
                    Transition::EdgeTransition(_, _) => Some(cmp::Ordering::Greater),
                    Transition::EpsilonTransition(_) => Some(cmp::Ordering::Equal)
                }
            }
        }
    }
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


    pub fn next_state_node(&self, state: State, node: &Node) -> Option<State> {
        if state >= self.transitions.len() {
            panic!("Tried to transition from state that wasn't in this automata")
        }
        for transition in self.transitions.get(state).unwrap() {
            match *transition {
                Transition::NodeTransition(ref matcher, new_state) => {
                    if matcher.matches(node) {
                        return Some(new_state)
                    }
                },
                Transition::EpsilonTransition(new_state) => {
                    if let Some(sub_state) = self.next_state_node(new_state, node) {
                        return Some(sub_state)
                    }
                },
                _ => {}
            }
        }
        return None
    }

    pub fn next_state_edge(&self, state: State, edge: &Edge) -> Option<State> {
        if state >= self.transitions.len() {
            panic!("Tried to transition from state that wasn't in this automata")
        }
        for transition in self.transitions.get(state).unwrap() {
            match *transition {
                Transition::EdgeTransition(label_match, new_state) => {
                    if edge.labels.contains(label_match) {
                        return Some(new_state)
                    }
                },
                Transition::EpsilonTransition(new_state) => {
                    if let Some(sub_state) = self.next_state_edge(new_state, edge) {
                        return Some(sub_state)
                    }
                },
                _ => {}
            }
        }
        return None
    }

    /// Create a MatchingAutomaton that accepts exactly the paths described by `matcher`.
    pub fn from_path_matcher(matcher: &'a PathMatcher) -> MatchingAutomaton<'a> {
        let mut transitions = vec![Vec::new()];
        let factory = StateGenerator::new();
        let start_state = factory.next_state();
        MatchingAutomaton::process_path_matcher(matcher, &factory, start_state, start_state, &mut transitions);
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
            PathMatcher::To(ref node, ref child_matcher) => {
                let tail_state = MatchingAutomaton::process_path_matcher(child_matcher, factory, starting_state, final_state, transitions);
                MatchingAutomaton::from_node(factory, tail_state, node, transitions)
            },
            PathMatcher::Or(ref option1, ref option2) => {
                MatchingAutomaton::from_or(factory, starting_state, final_state, option1, option2, transitions)
            }
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
        let final_state = factory.next_state();
        let label_transitions = labels.iter().map(|label| Transition::EdgeTransition(label, final_state));
        transitions[starting_state].extend(label_transitions);
        transitions.push(Vec::new());
        final_state
    }

    fn from_node(factory: &StateGenerator, starting_state: State, node: &'a NodeMatcher, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        let final_state = factory.next_state();
        transitions[starting_state].push(Transition::NodeTransition(node, final_state));
        transitions.push(Vec::new());
        final_state
    }

    fn from_or(factory: &StateGenerator, starting_state: State, final_state: State, option1: &'a PathMatcher, option2: &'a PathMatcher, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        let final1 = MatchingAutomaton::process_path_matcher(option1, factory, starting_state, final_state, transitions);
        let final2 = MatchingAutomaton::process_path_matcher(option2, factory, starting_state, final_state, transitions);
        transitions[final1].push(Transition::EpsilonTransition(final2));
        final2
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
            let new_states = MatchingAutomaton::duplicate_states(factory, &the_states, &starting_state, &final_state, &tail_state, transitions);
            last_start = tail_state;
            tail_state = new_states.get(&final_state).unwrap().clone();
            println!("tail state: {:?}", tail_state);
            println!("transitions: {:?}", transitions);
        }
        if let Some(maximum) = maximum {
            let mut skippable_starts = Vec::new();
            for _ in minimum + 1..maximum {
                let new_states = MatchingAutomaton::duplicate_states(factory, &the_states, &starting_state, &final_state, &tail_state, transitions);
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

    fn duplicate_states(factory: &StateGenerator, states: &HashSet<State>, starting_state: &State, final_state: &State, tail_state: &State, transitions: &mut Vec<Vec<Transition<'a>>>) -> HashMap<State, State> {
        let mut state_lookup = HashMap::new();
        // Create a new state for each existing state
        for state in states.iter() {
            // We match the starting state of the new states to the final state of the old states
            if state == starting_state {
                state_lookup.insert(*state, *tail_state);
            } else {
                let new_state = factory.next_state();
                state_lookup.insert(*state, new_state);
            }

        }
        let mut new_start = Vec::new();
        println!("state_lookup: {:?}", state_lookup);
        // Now that we've created the states, we iterate through the old states in the same
        // order as above, and copy the transitions
        for state in states.iter() {
            if state != final_state {
                let new_transitions = transitions[*state].iter().map(|transition| {
                    println!("{:?}", transition);
                    match transition {
                        &Transition::NodeTransition(node, ref new_state) => {
                            Transition::NodeTransition(node, state_lookup[new_state])
                        },
                        &Transition::EdgeTransition(edge, ref new_state) => {
                            Transition::EdgeTransition(edge, state_lookup[new_state])
                        },
                        &Transition::EpsilonTransition(ref new_state) => {
                            Transition::EpsilonTransition(state_lookup[new_state])
                        }
                    }
                }).collect();
                if state == starting_state {
                    new_start = new_transitions;
                } else {
                    transitions.push(new_transitions);
                }
            } else {
                transitions.push(Vec::new());
            }
        }
        transitions[*tail_state] = new_start;
        state_lookup
    }

    /// Converts the automata to a cannonical form.  Any two isomorphic automata are guaranteed
    /// to have the same cannonical form, and thus they can be compared.  Does not consume
    /// the automata.
    ///
    /// Largely designed for testing.
    ///
    /// The algorithm performs a breadth first search of the automata, starting with the
    /// start state, and moving through each transition in 'lexigraphical' order.
    /// Lexigraphical order is defined as node transitions first, then edge transitions
    /// then epsilon transitions.  The types of transitions themselves are ordered in Rust's
    /// default enum way.
    ///
    /// # Panics
    /// Panics when the automata contains unreachable states.
    pub fn to_cannonical(&self) -> MatchingAutomaton<'a> {
        let mut new_to_old = HashMap::new();
        let mut old_to_new = HashMap::new();
        let factory = StateGenerator::new();
        let mut open_set = VecDeque::new();
        open_set.push_back(0);
        while !open_set.is_empty() {
            let current = open_set.pop_front().unwrap();
            if !old_to_new.contains_key(&current) {
                let new_state = factory.next_state();
                new_to_old.insert(new_state, current);
                old_to_new.insert(current, new_state);
                let mut sorted_transitions = self.transitions[current].clone();
                sorted_transitions.sort();
                for transition in sorted_transitions {
                    let child = match transition {
                        Transition::NodeTransition(_, child) => child,
                        Transition::EdgeTransition(_, child) => child,
                        Transition::EpsilonTransition(child) => child
                    };
                    if !old_to_new.contains_key(&child) {
                        open_set.push_back(child);
                    }
                }
            }
        }
        let mut new_transitions = Vec::new();
        for new_state in 0..self.transitions.len() {
            let mut sorted_transitions = self.transitions[new_to_old[&new_state]].clone();
            sorted_transitions.sort();
            let my_transitions = sorted_transitions.iter().map(|transition| {
                match transition {
                    &Transition::NodeTransition(node, ref old_state) => {
                        Transition::NodeTransition(node, old_to_new[old_state])
                    },
                    &Transition::EdgeTransition(edge, ref old_state) => {
                        Transition::EdgeTransition(edge, old_to_new[old_state])
                    },
                    &Transition::EpsilonTransition(ref old_state) => {
                        Transition::EpsilonTransition(old_to_new[old_state])
                    }
                }
            }).collect();
            new_transitions.push(my_transitions);
        }
        MatchingAutomaton {
            transitions: new_transitions
        }
    }

}

impl StateGenerator {
    fn new() -> StateGenerator {
        StateGenerator{
            max_state: Cell::new(0)
        }
    }

    fn next_state(&self) -> State {
        let current = self.max_state.get();
        self.max_state.set(current + 1);
        current
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
    use super::{node_with_prop, node_with_id, MatchingAutomaton, Transition, NodeMatcher, ANY, PathMatcher};
    use ::{PropVal, Graph};
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
        let path_matcher = node_with_id(6).connected_by_label("label1").to(NodeMatcher::Any).then_label("label2");
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

    #[test]
    fn test_node_then_labels_repeat() {
        let expected_label1 = "label1".to_string().into_boxed_str();
        let expected_label2 = "label2".to_string().into_boxed_str();
        let path_matcher = node_with_id(6).connected_by_label("label1").to(NodeMatcher::Any).then_label("label2").repeat(Some(2), None);
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        let expected_node = node_with_id(6);
        println!("Automaton: {:?}", automaton);
        println!("Cannonical Automaton: {:?}", automaton.to_cannonical());
        let expected_transitions = vec![
            vec![Transition::NodeTransition(&expected_node, 1)],
            vec![Transition::EdgeTransition(&expected_label1, 2)],
            vec![Transition::NodeTransition(&ANY, 3)],
            vec![Transition::EdgeTransition(&expected_label2, 4)],
            vec![Transition::NodeTransition(&expected_node, 5)],
            vec![Transition::EdgeTransition(&expected_label1, 6)],
            vec![Transition::NodeTransition(&ANY, 7)],
            vec![Transition::EdgeTransition(&expected_label2, 8)],
            vec![Transition::EpsilonTransition(4)]
        ];
        assert!(MatchingAutomaton{
            transitions: expected_transitions
        } == automaton.to_cannonical())
    }

    #[test]
    fn test_multiple_repeats() {
        let path_matcher = node_with_id(6).connected_by_label("label1").repeat(None, Some(4));
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        println!("Automaton: {:?}", automaton);
        println!("Cannonical Automaton: {:?}", automaton.to_cannonical());
        let expected_node = node_with_id(6);
        let expected_label = "label1".to_string().into_boxed_str();
        let expected_transitions = vec![
            vec![Transition::NodeTransition(&expected_node, 1), Transition::EpsilonTransition(2)],
            vec![Transition::EdgeTransition(&expected_label, 3)],
            vec![],
            vec![Transition::NodeTransition(&expected_node, 4), Transition::EpsilonTransition(2)],
            vec![Transition::EdgeTransition(&expected_label, 5)],
            vec![Transition::NodeTransition(&expected_node, 6), Transition::EpsilonTransition(2)],
            vec![Transition::EdgeTransition(&expected_label, 7)],
            vec![Transition::NodeTransition(&expected_node, 8), Transition::EpsilonTransition(2)],
            vec![Transition::EdgeTransition(&expected_label, 2)]

        ];

        assert!(MatchingAutomaton{
            transitions: expected_transitions
        } == automaton.to_cannonical())
    }

    #[test]
    fn test_matching_or() {
        let path1 = node_with_id(6).connected_by_label("label1");
        let path2 = node_with_id(5).connected_by_label("label2");
        let path_matcher = PathMatcher::Or(Box::new(path1), Box::new(path2));
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        println!("Automaton: {:?}", automaton);
        println!("Cannonical Automaton: {:?}", automaton.to_cannonical());
        let expected_node1 = node_with_id(6);
        let expected_label1 = "label1".to_string().into_boxed_str();
        let expected_node2 = node_with_id(5);
        let expected_label2 = "label2".to_string().into_boxed_str();

        let expected_transitions = vec![
            vec![Transition::NodeTransition(&expected_node2, 1), Transition::NodeTransition(&expected_node1, 2)],
            vec![Transition::EdgeTransition(&expected_label2, 3)],
            vec![Transition::EdgeTransition(&expected_label1, 4)],
            vec![],
            vec![Transition::EpsilonTransition(3)],

        ];

        assert!(MatchingAutomaton{
            transitions: expected_transitions
        } == automaton.to_cannonical())
    }
}
