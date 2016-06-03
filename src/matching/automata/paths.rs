use super::{State, Transition};
use std::collections::{HashSet, VecDeque};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
enum Direction {
    Forward,
    Backward
}
#[derive(Eq, Debug)]
struct GraphNodeWithParent {
    state: State,
    direction: Direction,
    parent: Option<Arc<GraphNodeWithParent>>
}

#[derive(Eq, Debug)]
struct GraphNode {
    state: State,
    direction: Direction
}

impl PartialEq for GraphNodeWithParent {
    fn eq(&self, other: &GraphNodeWithParent) -> bool {
        self.state == other.state
    }
}

impl Hash for GraphNodeWithParent {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.state.hash(state);
    }
}

impl PartialEq for GraphNode {
    fn eq(&self, other: &GraphNode) -> bool {
        self.state == other.state
    }
}

impl Hash for GraphNode {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.state.hash(state);
    }
}


/// Finds all of the states that must be gone through, regardless of the
pub fn find_mandatory_states<'a>(transitions: &Vec<Vec<Transition<'a>>>, final_states: &HashSet<State>) -> HashSet<State> {
    let mut mandatory_states = HashSet::new();
    // Find the shortest path between the starting state and any final state
    let rev = reverse_transitions(transitions);
    let shortest_path = find_shortest_path(transitions, &rev, final_states);

    let mut end = final_states.clone();
    // We're going to move through the path starting at the back so if we find a mandatory state,
    // we can use that as the new end
    for state in shortest_path.into_iter().rev() {
        if !path_exists_without(state, 0, transitions, &rev, &end) {
            // If there's no path without this state, then it's mandatory
            mandatory_states.insert(state);

            // No need to go all the way to the end.  If this state is mandatory, then
            // all paths will go through it, so we can end our search there.
            end = HashSet::new();
            end.insert(state);
        }
    }
    mandatory_states
}

fn reverse_transitions<'a>(transitions: &Vec<Vec<Transition<'a>>>) -> Vec<Vec<State>> {
    let mut rev_trans = Vec::with_capacity(transitions.len());
    for _ in 0..transitions.len() {
        rev_trans.push(Vec::new());
    }
    for (src, state_transitions) in transitions.iter().enumerate() {
        for transition in state_transitions {
            let dst = transition.get_state();
            rev_trans[dst].push(src);
        }
    }
    rev_trans
}


fn find_shortest_path<'a>(transitions: &Vec<Vec<Transition<'a>>>, reverse_transitions: &Vec<Vec<State>>, final_states: &HashSet<State>) -> Vec<State> {

    let mut open_set = VecDeque::new();
    open_set.push_back(Arc::new(GraphNodeWithParent{state: 0, direction: Direction::Forward, parent: None}));
    for state in final_states {
        open_set.push_back(Arc::new(GraphNodeWithParent{state: *state, direction: Direction::Backward, parent: None}));
    }
    let mut closed_set = HashSet::new();
    while !open_set.is_empty() {
        let current = open_set.pop_front().unwrap();
        if closed_set.contains(&current) {
            continue;
        }
        closed_set.insert(current.clone());
        match current.direction {
            Direction::Forward => {
                for child in transitions[current.state.clone()].iter().map(Transition::get_state) {
                    let child_node = GraphNodeWithParent {
                        state: child,
                        direction: Direction::Forward,
                        parent: Some(current.clone())
                    };
                    match closed_set.get(&child_node) {
                        Some(ref other) => {
                            if other.direction == Direction::Backward {
                                return create_path(Arc::new(child_node), (*other).clone());
                            }
                        }, None => {
                            open_set.push_back(Arc::new(child_node))
                        }
                    }
                }
            },
            Direction::Backward => {
                for child in reverse_transitions[current.state.clone()].iter() {
                    let child_node = GraphNodeWithParent {
                        state: *child,
                        direction: Direction::Backward,
                        parent: Some(current.clone())
                    };
                    match closed_set.get(&child_node) {
                        Some(ref other) => {
                            if other.direction == Direction::Forward {
                                return create_path((*other).clone(), Arc::new(child_node));
                            }
                        }, None => {
                            open_set.push_back(Arc::new(child_node))
                        }
                    }
                }
            }
        }



    }
    vec![0] // only happens if the starting node is one of the final nodes
}

fn create_path(forward_node: Arc<GraphNodeWithParent>, backward_node: Arc<GraphNodeWithParent>) -> Vec<State> {
    let mut result = Vec::new();
    let mut current_node = Some(backward_node);
    while let Some(node) = current_node {
        result.push(node.state.clone());
        current_node = node.parent.clone()
    }
    current_node = forward_node.parent.clone();
    while let Some(node) = current_node {
        result.insert(0, node.state.clone());
        current_node = node.parent.clone();
    }
    result
}

fn path_exists_without<'a>(missing_state: State, start_state: State, transitions: &Vec<Vec<Transition<'a>>>, reverse_transitions: &Vec<Vec<State>>, final_states: &HashSet<State>) -> bool {
    let mut open_set = VecDeque::new();
    open_set.push_back(Arc::new(GraphNode{state: start_state, direction: Direction::Forward}));
    for state in final_states {
        if *state != missing_state {
            open_set.push_back(Arc::new(GraphNode{state: *state, direction: Direction::Backward}));
        }
    }
    let mut closed_set = HashSet::new();
    while !open_set.is_empty() {
        let current = open_set.pop_front().unwrap();
        if closed_set.contains(&current) {
            continue;
        }
        closed_set.insert(current.clone());
        match current.direction {
            Direction::Forward => {
                for child in transitions[current.state.clone()].iter().map(Transition::get_state) {
                    if child == missing_state {
                        continue;
                    }
                    let child_node = GraphNode {
                        state: child,
                        direction: Direction::Forward,
                    };
                    match closed_set.get(&child_node) {
                        Some(ref other) => {
                            if other.direction == Direction::Backward {
                                return true
                            }
                        }, None => {
                            open_set.push_back(Arc::new(child_node))
                        }
                    }
                }
            },
            Direction::Backward => {
                for child in reverse_transitions[current.state.clone()].iter() {
                    if *child == missing_state {
                        continue;
                    }
                    let child_node = GraphNode {
                        state: *child,
                        direction: Direction::Backward,
                    };
                    match closed_set.get(&child_node) {
                        Some(ref other) => {
                            if other.direction == Direction::Forward {
                                return true
                            }
                        }, None => {
                            open_set.push_back(Arc::new(child_node))
                        }
                    }
                }
            }
        }
    }
    false
}

#[cfg(test)]
mod test {
use matching::{NodeMatcher, EdgeMatcher};
use super::super::{Transition};
use super::{reverse_transitions, find_shortest_path, path_exists_without, find_mandatory_states};
use std::collections::HashSet;

#[test]
fn test_reverse_transitions() {
    let transitions = vec![
        vec![Transition::NodeTransition(NodeMatcher::Any, 1)],
        vec![Transition::EdgeTransition(EdgeMatcher::Any, 2)],
        vec![Transition::EpsilonTransition(4)],
        vec![Transition::EpsilonTransition(1)],
        vec![Transition::EdgeTransition(EdgeMatcher::Any, 3), Transition::NodeTransition(NodeMatcher::Any, 5)],
        vec![]
    ];
    let rev = reverse_transitions(&transitions);
    assert_eq!(rev, vec![
        vec![],
        vec![0, 3],
        vec![1],
        vec![4],
        vec![2],
        vec![4]
        ])
}

    #[test]
    fn test_get_shortest_path() {
        {
            let transitions = vec![
                vec![Transition::EpsilonTransition(1)], // 0
                vec![Transition::EpsilonTransition(2), Transition::EpsilonTransition(3)], // 1
                vec![Transition::EpsilonTransition(4)], // 2
                vec![Transition::EpsilonTransition(6)], // 3
                vec![Transition::EpsilonTransition(4), Transition::EpsilonTransition(7), Transition::EpsilonTransition(5)], // 4
                vec![Transition::EpsilonTransition(8)], // 5
                vec![Transition::EpsilonTransition(9)], // 6
                vec![Transition::EpsilonTransition(7)], // 7
                vec![Transition::EpsilonTransition(7),Transition::EpsilonTransition(10)], // 8
                vec![Transition::EpsilonTransition(11)], // 9
                vec![], // 10
                vec![Transition::EpsilonTransition(12)], // 11
                vec![Transition::EpsilonTransition(7)], // 12
            ];
            let mut final_states = HashSet::new();
            final_states.insert(7);
            final_states.insert(10);
            final_states.insert(12);
            let path = find_shortest_path(&transitions, &reverse_transitions(&transitions), &final_states);
            assert_eq!(path, vec![0, 1, 2, 4, 7]);
        }
        {
            let transitions = vec![vec![Transition::EpsilonTransition(1)], vec![]];
            let mut final_states = HashSet::new();
            final_states.insert(1);
            let path = find_shortest_path(&transitions, &reverse_transitions(&transitions), &final_states);
            assert_eq!(path, vec![0, 1]);
        }

        {
            let transitions = vec![vec![]];
            let mut final_states = HashSet::new();
            final_states.insert(0);
            let path = find_shortest_path(&transitions, &reverse_transitions(&transitions), &final_states);
            assert_eq!(path, vec![0]);
        }

    }

    #[test]
    fn test_path_exists() {
        let transitions = vec![
            vec![Transition::EpsilonTransition(1)], // 0
            vec![Transition::EpsilonTransition(2), Transition::EpsilonTransition(3)], // 1
            vec![Transition::EpsilonTransition(4)], // 2
            vec![Transition::EpsilonTransition(6)], // 3
            vec![Transition::EpsilonTransition(4), Transition::EpsilonTransition(7), Transition::EpsilonTransition(5)], // 4
            vec![Transition::EpsilonTransition(8)], // 5
            vec![Transition::EpsilonTransition(9)], // 6
            vec![Transition::EpsilonTransition(7)], // 7
            vec![Transition::EpsilonTransition(7),Transition::EpsilonTransition(10)], // 8
            vec![Transition::EpsilonTransition(11)], // 9
            vec![], // 10
            vec![Transition::EpsilonTransition(12)], // 11
            vec![Transition::EpsilonTransition(7)], // 12
        ];
        let mut final_states = HashSet::new();
        final_states.insert(7);
        final_states.insert(10);
        final_states.insert(12);
        let rev = reverse_transitions(&transitions);
        assert!(!path_exists_without(1, 0, &transitions, &rev, &final_states));
        assert!(path_exists_without(2, 0, &transitions, &rev, &final_states));
        assert!(!path_exists_without(9, 6, &transitions, &rev, &final_states));
        assert!(path_exists_without(8, 6, &transitions, &rev, &final_states));
        assert!(!path_exists_without(12, 6, &transitions, &rev, &final_states));
    }

    #[test]
    fn test_mandatory_states() {
        {
            let transitions = vec![
                vec![Transition::EpsilonTransition(1)], // 0
                vec![Transition::EpsilonTransition(2), Transition::EpsilonTransition(3)], // 1
                vec![Transition::EpsilonTransition(4)], // 2
                vec![Transition::EpsilonTransition(6)], // 3
                vec![Transition::EpsilonTransition(4), Transition::EpsilonTransition(7), Transition::EpsilonTransition(5)], // 4
                vec![Transition::EpsilonTransition(8)], // 5
                vec![Transition::EpsilonTransition(9)], // 6
                vec![Transition::EpsilonTransition(7)], // 7
                vec![Transition::EpsilonTransition(7),Transition::EpsilonTransition(10)], // 8
                vec![Transition::EpsilonTransition(11)], // 9
                vec![], // 10
                vec![Transition::EpsilonTransition(12)], // 11
                vec![], // 12
            ];
            let mut final_states = HashSet::new();
            final_states.insert(7);
            final_states.insert(10);
            final_states.insert(12);
            let mut expected_mandatory = HashSet::new();
            expected_mandatory.insert(0);
            expected_mandatory.insert(1);
            assert_eq!(find_mandatory_states(&transitions, &final_states), expected_mandatory);
        }
        {
            let transitions = vec![
                vec![Transition::NodeTransition(NodeMatcher::Any, 1)],
                vec![Transition::EdgeTransition(EdgeMatcher::Any, 2)],
                vec![Transition::EpsilonTransition(4)],
                vec![Transition::EpsilonTransition(1)],
                vec![Transition::EdgeTransition(EdgeMatcher::Any, 3), Transition::NodeTransition(NodeMatcher::Any, 5)],
                vec![]
            ];
            let mut final_states = HashSet::new();
            final_states.insert(5);
            let mut expected_mandatory = HashSet::new();
            expected_mandatory.insert(0);
            expected_mandatory.insert(1);
            expected_mandatory.insert(2);
            expected_mandatory.insert(4);
            expected_mandatory.insert(5);
            assert_eq!(find_mandatory_states(&transitions, &final_states), expected_mandatory);
        }
    }
}
