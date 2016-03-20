use std::cell::Cell;
use std::fmt;
use std::cmp;
use std::collections::{HashMap, HashSet, VecDeque};
use super::{NodeMatcher, PathMatcher, EdgeMatcher};
use ::{Node};
use queries::{ASTPath, ASTEdge, EdgeToNode};

#[derive(PartialEq)]
/// An automata that can match paths in a graph.
pub struct MatchingAutomaton<'a> {
    transitions: Vec<Vec<Transition<'a>>>,
    final_states: HashSet<State>
}

pub type State = usize;

#[derive(PartialEq, Debug, Eq, Ord, Clone)]
enum Transition<'a> {
    NodeTransition(NodeMatcher, State),
    EdgeTransition(EdgeMatcher<'a>, State),
    EpsilonTransition(State), // allows moving to the next state without consuming any input
}

struct StateGenerator {
    max_state: Cell<State>
}



impl<'a> fmt::Debug for MatchingAutomaton<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "{{"));
        for (state, transition) in self.transitions.iter().enumerate() {
            if self.final_states.contains(&state) {
                try!(writeln!(f, " *{}: {:?}", state, transition));
            } else {
                try!(writeln!(f, "  {}: {:?}", state, transition));
            }
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

    pub fn next_state_edge(&self, state: State, labels: &HashSet<Box<str>>) -> Option<State> {
        if state >= self.transitions.len() {
            panic!("Tried to transition from state that wasn't in this automata")
        }
        for transition in self.transitions.get(state).unwrap() {
            match *transition {
                Transition::EdgeTransition(ref label_match, ref new_state) => {
                    if label_match.matches(labels) {
                        return Some(*new_state)
                    }
                },
                Transition::EpsilonTransition(new_state) => {
                    if let Some(sub_state) = self.next_state_edge(new_state, labels) {
                        return Some(sub_state)
                    }
                },
                _ => {}
            }
        }
        return None
    }

    pub fn is_complete(&self, state: State) -> bool {
        return self.final_states.contains(&state);
    }

    pub fn from_path_expression(path: ASTPath<'a>) -> MatchingAutomaton<'a> {
        let mut transitions = vec![Vec::new()];
        let factory = StateGenerator::new();
        let starting_state = factory.next_state();
        let final_state = MatchingAutomaton::process_path_expression(path, &factory, starting_state, &mut transitions);
        let final_states = MatchingAutomaton::find_final_states(final_state, &transitions);
        MatchingAutomaton{transitions:transitions, final_states: final_states}
    }

    fn process_path_expression(path: ASTPath<'a>, factory: &StateGenerator, starting_state: State, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        let head_state = factory.next_state();
        transitions[starting_state].push(Transition::NodeTransition(path.head, head_state));
        transitions.push(Vec::new());
        MatchingAutomaton::process_edge_to_node(path.tail, factory, head_state, transitions)
    }

    fn process_edge_to_node(e2n: EdgeToNode<'a>, factory: &StateGenerator, starting_state: State,  transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        match e2n {
            EdgeToNode::Standard {
                node,
                edge,
                tail,
            } => {
                let edge_state = factory.next_state();
                let node_state = factory.next_state();
                let edge_transition = match edge {
                    ASTEdge::LabelList(ref labels) => Transition::EdgeTransition(EdgeMatcher::LabelList(labels.clone()), edge_state),
                    ASTEdge::Any => Transition::EdgeTransition(EdgeMatcher::Any, edge_state)
                };
                transitions[starting_state].push(edge_transition);
                transitions.push(vec![Transition::NodeTransition(node.clone(), node_state)]);
                transitions.push(Vec::new());
                match tail {
                    Some(tail) => MatchingAutomaton::process_edge_to_node(*tail, factory, node_state, transitions),
                    None => node_state
                }
            },
            EdgeToNode::Repeated {
                min,
                max,
                repeated,
                tail
            } => {
                let tail_repeated_state = MatchingAutomaton::process_edge_to_node(*repeated, factory, starting_state, transitions);
                let tail_state = MatchingAutomaton::repeat(factory, starting_state, tail_repeated_state, Some(min), max, transitions);
                match tail {
                    Some(tail) => MatchingAutomaton::process_edge_to_node(*tail, factory, tail_state, transitions),
                    None => tail_state
                }
            }
        }
    }

    /// Create a MatchingAutomaton that accepts exactly the paths described by `matcher`.
    pub fn from_path_matcher(matcher: &'a PathMatcher) -> MatchingAutomaton<'a> {
        let mut transitions = vec![Vec::new()];
        let factory = StateGenerator::new();
        let start_state = factory.next_state();
        let final_state = MatchingAutomaton::process_path_matcher(matcher, &factory, start_state, start_state, &mut transitions);
        let final_states = MatchingAutomaton::find_final_states(final_state, &transitions);
        MatchingAutomaton{transitions:transitions, final_states: final_states}

    }

    fn process_path_matcher(matcher: &'a PathMatcher<'a>, factory: &StateGenerator, starting_state: State, final_state: State, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        match *matcher {
            PathMatcher::Labels(ref labels, ref node) => {
                MatchingAutomaton::from_node_and_labels(factory, starting_state, node.clone(), labels.clone(), transitions)
            },
            PathMatcher::Repeat(miniumum, maximum, ref child_matcher) => {
                let tail_state = MatchingAutomaton::process_path_matcher(child_matcher, factory, starting_state, final_state, transitions);
                MatchingAutomaton::repeat(factory, starting_state, tail_state, miniumum, maximum, transitions)
            },
            PathMatcher::ThenLabels(ref labels, ref child_matcher) => {
                let tail_state = MatchingAutomaton::process_path_matcher(child_matcher, factory, starting_state, final_state, transitions);
                MatchingAutomaton::from_labels(factory, tail_state, labels.clone(), transitions)
            },
            PathMatcher::To(ref node, ref child_matcher) => {
                let tail_state = MatchingAutomaton::process_path_matcher(child_matcher, factory, starting_state, final_state, transitions);
                MatchingAutomaton::from_node(factory, tail_state, node.clone(), transitions)
            },
            PathMatcher::Or(ref option1, ref option2) => {
                MatchingAutomaton::from_or(factory, starting_state, final_state, option1, option2, transitions)
            }
        }
    }

    fn from_node_and_labels(factory: &StateGenerator, starting_state: State, node: NodeMatcher, labels: Vec<&'a str>, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        let node_state = factory.next_state();
        let final_state = factory.next_state();
        transitions[starting_state].push(Transition::NodeTransition(node, node_state));
        transitions.push(vec![Transition::EdgeTransition(EdgeMatcher::LabelList(labels), final_state)]);
        transitions.push(Vec::new());
        final_state
    }

    fn from_labels(factory: &StateGenerator, starting_state: State, labels: Vec<&'a str>, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
        let final_state = factory.next_state();
        transitions[starting_state].push(Transition::EdgeTransition(EdgeMatcher::LabelList(labels), final_state));
        transitions.push(Vec::new());
        final_state
    }

    fn from_node(factory: &StateGenerator, starting_state: State, node: NodeMatcher, transitions: &mut Vec<Vec<Transition<'a>>>) -> State {
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
            let lower_bound = if minimum == 0 {1} else {minimum};
            for _ in lower_bound..maximum {
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
                        &Transition::NodeTransition(ref node, ref new_state) => {
                            Transition::NodeTransition(node.clone(), state_lookup[new_state])
                        },
                        &Transition::EdgeTransition(ref edge, ref new_state) => {
                            Transition::EdgeTransition(edge.clone(), state_lookup[new_state])
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

    fn find_final_states(final_state: State, transitions: &Vec<Vec<Transition<'a>>>) -> HashSet<State> {
        let mut final_states = HashSet::new();
        final_states.insert(final_state);
        let mut found = true;
        while found {
            found = false;
            for (state, state_transitions) in transitions.iter().enumerate() {
                if !final_states.contains(&state) {
                    for transition in state_transitions.iter() {
                        if let &Transition::EpsilonTransition(new_state) = transition {
                            if final_states.contains(&new_state) {
                                final_states.insert(state);
                                found = true;
                                break;
                            }
                        }
                    }
                }
            }
        }
        final_states
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
                    &Transition::NodeTransition(ref node, ref old_state) => {
                        Transition::NodeTransition(node.clone(), old_to_new[old_state])
                    },
                    &Transition::EdgeTransition(ref edge, ref old_state) => {
                        Transition::EdgeTransition(edge.clone(), old_to_new[old_state])
                    },
                    &Transition::EpsilonTransition(ref old_state) => {
                        Transition::EpsilonTransition(old_to_new[old_state])
                    }
                }
            }).collect();
            new_transitions.push(my_transitions);
        }
        let mut new_finals = HashSet::new();
        for old_state in self.final_states.iter() {
            new_finals.insert(old_to_new[old_state]);
        }
        MatchingAutomaton {
            transitions: new_transitions,
            final_states: new_finals
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

#[cfg(test)]
mod tests {
    use super::{MatchingAutomaton, Transition};
    use super::super::{node_with_id, NodeMatcher, PathMatcher, EdgeMatcher};
    use std::collections::HashSet;
    use std::iter::FromIterator;

    macro_rules! make_matcher {
        ($e:expr) => (EdgeMatcher::LabelList(vec![$e]));
    }

    macro_rules! final_set {
        ($( $x:expr), *) => {
            HashSet::from_iter(vec![$( $x ,)*])
        };
    }

    #[test]
    fn test_node_label_match() {
        let path_matcher = node_with_id(6).connected_by_label("label1");
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        let expected_node = node_with_id(6);
        let expected_label = make_matcher!("label1");
        let expected_transitions = vec![
            vec![Transition::NodeTransition(expected_node, 1)],
            vec![Transition::EdgeTransition(expected_label, 2)],
            vec![]
        ];
        assert!(MatchingAutomaton{
            transitions: expected_transitions,
            final_states: final_set!(2)
        } == automaton)
    }

    #[test]
    fn test_node_multiple_label_match() {
        let edge_matcher = EdgeMatcher::LabelList(vec!["label1", "label2", "label3"]);
        let path_matcher = node_with_id(6).connected_by_one_of(vec!["label1", "label2", "label3"]);
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        let expected_node = node_with_id(6);

        let expected_transitions = vec![
            vec![Transition::NodeTransition(expected_node, 1)],
            vec![Transition::EdgeTransition(edge_matcher, 2)],
            vec![]
        ];
        assert!(MatchingAutomaton{
            transitions: expected_transitions,
            final_states: final_set!(2)
        } == automaton)
    }

    #[test]
    fn test_node_then_labels() {
        let expected_label1 = make_matcher!("label1");
        let expected_label2 = make_matcher!("label2");
        let path_matcher = node_with_id(6).connected_by_label("label1").to(NodeMatcher::Any).then_label("label2");
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        let expected_node = node_with_id(6);

        let expected_transitions = vec![
            vec![Transition::NodeTransition(expected_node, 1)],
            vec![Transition::EdgeTransition(expected_label1, 2)],
            vec![Transition::NodeTransition(NodeMatcher::Any, 3)],
            vec![Transition::EdgeTransition(expected_label2, 4)],
            vec![]
        ];
        assert!(MatchingAutomaton{
            transitions: expected_transitions,
            final_states: final_set!(4)
        } == automaton)
    }

    #[test]
    fn test_node_then_labels_repeat() {
        let expected_label1 = make_matcher!("label1");
        let expected_label2 = make_matcher!("label2");
        let path_matcher = node_with_id(6).connected_by_label("label1").to(NodeMatcher::Any).then_label("label2").repeat(Some(2), None);
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        let expected_node = node_with_id(6);
        println!("Automaton: {:?}", automaton);
        println!("Cannonical Automaton: {:?}", automaton.to_cannonical());
        let expected_transitions = vec![
            vec![Transition::NodeTransition(expected_node.clone(), 1)],
            vec![Transition::EdgeTransition(expected_label1.clone(), 2)],
            vec![Transition::NodeTransition(NodeMatcher::Any, 3)],
            vec![Transition::EdgeTransition(expected_label2.clone(), 4)],
            vec![Transition::NodeTransition(expected_node.clone(), 5)],
            vec![Transition::EdgeTransition(expected_label1.clone(), 6)],
            vec![Transition::NodeTransition(NodeMatcher::Any, 7)],
            vec![Transition::EdgeTransition(expected_label2.clone(), 8)],
            vec![Transition::EpsilonTransition(4)]
        ];
        assert!(MatchingAutomaton{
            transitions: expected_transitions,
            final_states: final_set!(8)
        } == automaton.to_cannonical())
    }

    #[test]
    fn test_multiple_repeats() {
        let path_matcher = node_with_id(6).connected_by_label("label1").repeat(None, Some(4));
        let automaton = MatchingAutomaton::from_path_matcher(&path_matcher);
        println!("Automaton: {:?}", automaton);
        println!("Cannonical Automaton: {:?}", automaton.to_cannonical());
        let expected_node = node_with_id(6);
        let expected_label =make_matcher!("label1");
        let expected_transitions = vec![
            vec![Transition::NodeTransition(expected_node.clone(), 1), Transition::EpsilonTransition(2)],
            vec![Transition::EdgeTransition(expected_label.clone(), 3)],
            vec![],
            vec![Transition::NodeTransition(expected_node.clone(), 4), Transition::EpsilonTransition(2)],
            vec![Transition::EdgeTransition(expected_label.clone(), 5)],
            vec![Transition::NodeTransition(expected_node.clone(), 6), Transition::EpsilonTransition(2)],
            vec![Transition::EdgeTransition(expected_label.clone(), 7)],
            vec![Transition::NodeTransition(expected_node.clone(), 8), Transition::EpsilonTransition(2)],
            vec![Transition::EdgeTransition(expected_label.clone(), 2)]

        ];

        assert!(MatchingAutomaton{
            transitions: expected_transitions,
            final_states: final_set!(0, 2, 3, 5, 7)
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
        let expected_label1 = make_matcher!("label1");
        let expected_node2 = node_with_id(5);
        let expected_label2 = make_matcher!("label2");

        let expected_transitions = vec![
            vec![Transition::NodeTransition(expected_node2, 1), Transition::NodeTransition(expected_node1, 2)],
            vec![Transition::EdgeTransition(expected_label2, 3)],
            vec![Transition::EdgeTransition(expected_label1, 4)],
            vec![],
            vec![Transition::EpsilonTransition(3)],

        ];

        assert!(MatchingAutomaton{
            transitions: expected_transitions,
            final_states: final_set!(3, 4)
        } == automaton.to_cannonical())
    }
}
