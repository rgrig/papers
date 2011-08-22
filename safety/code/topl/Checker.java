package topl;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

public class Checker {
    /*
        Some classes have a method {check()} that asserts if some object
        invariant is broken. These functions always return {true} so that
        you can say {assert x.check()} in case you want to skip them
        completely when assertions are not enabled.
     */

    static class Event {
        int id;
        Object[] values;

        Event(int id, Object[] values) {
            this.id = id;
            this.values = values;
            assert check();
        }

        boolean check() {
            assert values != null;
            for (Object o : values) {
                assert o != null;
            }
            return true;
        }
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
            r.stack = new HashMap<Integer, Object>();
            r.events = new ArrayDeque<Event>();
            return r;
        }

        State applyAction(Action action) {
            // TODO
            return this;
        }
    }

    interface Store {
        Object get(int variable);
        // TODO
    }

    interface Guard {
        boolean evaluate(Event event, Store store);
    }

    static class AndGuard implements Guard {
        Guard[] children;

        @Override
        public boolean evaluate(Event event, Store store) {
            for (Guard g : children) {
                if (!g.evaluate(event, store)) {
                    return false;
                }
            }
            return true;
        }
    }

    static class NotGuard implements Guard {
        Guard child;

        @Override
        public boolean evaluate(Event event, Store store) {
            return !child.evaluate(event, store);
        }
    }

    static class StoreEqualityGuard implements Guard {
        int eventIndex;
        int storeIndex;

        @Override
        public boolean evaluate(Event event, Store store) {
            return event.values[eventIndex] == store.get(storeIndex);
        }
    }

    static class TrueGuard implements Guard {
        @Override
        public boolean evaluate(Event event, Store store) {
            return true;
        }
    }

    static class Action {
        HashMap<Integer, Integer> assignments;
        Action() {
            assignments = new HashMap<Integer, Integer>();
        }
    }

    static class TransitionStep {
        // TODO: Consider bitmaps
        HashSet<Integer> eventIds;
        Guard guard;
        Action action;

        TransitionStep(int[] eventIds, Guard guard, Action action) {
            this.eventIds = new HashSet<Integer>();
            for (int e : eventIds) {
                this.eventIds.add(e);
            }
            this.guard = guard;
            this.action = action;
        }

        boolean evaluateGuard(Event event) {
            // TODO
            return false;
        }
        // TODO: applyAction consumes one event
    }

    static class Transition {
        TransitionStep[] steps;
        int target;

        Transition(TransitionStep oneStep, int target) {
            this.steps = new TransitionStep[]{oneStep};
            this.target = target;
        }
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
            assert check();
        }

        boolean check() {
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
            return true;
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

    public Checker(Automaton automaton) {
        this.automaton = automaton;
        this.states = new HashSet<State>();
        states.add(State.ofVertex(automaton.startVertex));
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
        Checker c = new Checker(new Automaton(0, 1,
                new Transition[][]{
                        new Transition[] {
                                new Transition(
                                        new TransitionStep(
                                                new int[]{},
                                                new TrueGuard(),
                                                new Action()), 1)},
                        new Transition[] {}
                }));
        c.check(new Event(0, new Object[]{}));
    }
}
/* TODO
    - write some tests
    - fill in the missing methods
    - implement persistent data structures
 */
// vim:sw=4:ts=4:
