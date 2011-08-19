package topl;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

public class Checker {
    static class Event {
        int id;
        Object[] values;
    }

    static class State {
        // TODO: Hashing
        int vertex;
        HashMap<Integer, Object> stack;
        ArrayDeque<Event> events;
        // The state {this} arose from {previousState} when {arrivalEvent} was seen.
        State previousState;
        Event arrivalEvent;

        static State ofVertex(int vertex) {
            State r = new State();
            r.vertex = vertex;
            return r;
        }

        State applyAction(Action action) {
            // TODO
            return this;
        }
    }

    static class Guard {
        // TODO
    }

    static class Action {
        // TODO
    }

    static class TransitionStep {
        // TODO: Consider bitmaps
        HashSet<Integer> eventIds;
        Guard guard;
        Action action;

        boolean evaluateGuard(Event event) {
            // TODO
            return false;
        }
        // TODO: applyAction consumes one event
    }

    static class Transition {
        TransitionStep[] steps;
        int target;
    }

    static class Automaton {
        int startVertex;
        int errorVertex;

        Transition[][] transitions;
            // {transitions[vertex]}  are the outgoing transitions of {vertex}

        private int maximumTransitionDepth = -1;

        Automaton(int startVertex, int errorVertex,
                Transition[][] transitions) {
            this.startVertex = startVertex;
            this.errorVertex = errorVertex;
            this.transitions = transitions;
            check();
        }

        void check() {
            assert 0 <= startVertex && startVertex < transitions.length;
            assert 0 <= errorVertex && errorVertex < transitions.length;
            assert transitions != null;
            for (Transition[] ts : transitions) {
                assert ts != null;
                for (Transition t : ts) {
                    assert t != null;
                    assert 0 <= t.target && t.target < transitions.length;
                    assert t.steps != null;
                    for (TransitionStep s : t.steps) {
                        assert s != null;
                        assert s.eventIds != null;
                        assert s.guard != null;
                        assert s.action != null;
                        // TODO(rgrig): Bounds for integers in guards/actions.
                    }
                }
            }
        }

        int maximumTransitionDepth() {
            if (maximumTransitionDepth == -1) {
                maximumTransitionDepth = 0;
                for (Transition[] ts : transitions) {
                    for (Transition t : ts) {
                        maximumTransitionDepth = Math.max(
                                maximumTransitionDepth, t.steps.length);
                    }
                }
            }
            return maximumTransitionDepth;
        }
    }

    private Automaton automaton;
    private HashSet<State> states;

    public Checker(Automaton automaton, int vertex) {
        this.automaton = automaton;
        this.states = new HashSet<State>();
        states.add(State.ofVertex(vertex));
    }

    void reportError() {
        // TODO
    }

    public void check(Event event) {
        HashSet<State> departedStates = new HashSet<State>();
        HashSet<State> arrivedStates = new HashSet<State>();
        for (State state : states) {
            state.events.addLast(event);
            if (state.events.size() < automaton.maximumTransitionDepth()) {
                continue;
            }
            for (Transition transition : automaton.transitions[state.vertex]) {
                // evaluate transition
                State stepState = state;
                int i = 0;
                Iterator<Event> j = state.events.iterator();
                for (; i < transition.steps.length; ++i) {
                    TransitionStep step = transition.steps[i];
                    Event stepEvent = j.next();
                    // if (!step.eventIds.contains(stepEvent.id)) break;
                    if (!step.evaluateGuard(stepEvent)) break;
                    stepState = stepState.applyAction(step.action);
                }
                // record transition
                if (i == transition.steps.length) {
                    departedStates.add(state);
                    arrivedStates.add(stepState);
                    // check for error state
                    if (transition.target == automaton.errorVertex)
                        // TODO
                        reportError();
                }
            }
            // perform transitions
            states.removeAll(departedStates);
            states.addAll(arrivedStates);
        }
    }

    /** Some basic tests. */
    public static void main(String[] args) {
        Automaton a = new Automaton(0, 1,
                new Transition[][]{
                        new Transition[] {},
                        new Transition[] {}
                });
    }
}
/* TODO
    - write some tests
    - fill in the missing methods
    - implement persistent data structures
 */
// vim:sw=4:ts=4:
