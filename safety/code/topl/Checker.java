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
        final int id;
        final Object[] values;

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
        final int vertex;
        final HashMap<Integer, Object> stack;
        final ArrayDeque<Event> events;
        // The state {this} arose from {previousState} when {arrivalEvent} was seen.
        final State previousState;
        final Event arrivalEvent;

        State(int vertex) {
            this.vertex = vertex;
            this.stack = new HashMap<Integer, Object>();
            this.events = new ArrayDeque<Event>();
            this.previousState = null;
            this.arrivalEvent = null;
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
        final Guard[] children;

        AndGuard(Guard[] children) {
            this.children = children;
        }

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
        final Guard child;

        NotGuard(Guard child) {
            this.child = child;
        }

        @Override
        public boolean evaluate(Event event, Store store) {
            return !child.evaluate(event, store);
        }
    }

    static class StoreEqualityGuard implements Guard {
        final int eventIndex;
        final int storeIndex;

        StoreEqualityGuard(int eventIndex, int storeIndex) {
            this.eventIndex = eventIndex;
            this.storeIndex = storeIndex;
        }

        @Override
        public boolean evaluate(Event event, Store store) {
            return event.values[eventIndex] == store.get(storeIndex);
        }
    }

    static class ConstantEqualityGuard implements Guard {
        final int eventIndex;
        final Object value;

        ConstantEqualityGuard(int eventIndex, Object value) {
            this.eventIndex = eventIndex;
            this.value = value;
        }

        @Override
        public boolean evaluate(Event event, Store store) {
            return (value == null)?
                event.values[eventIndex] == null :
                value.equals(event.values[eventIndex]);
        }
    }

    static class TrueGuard implements Guard {
        @Override
        public boolean evaluate(Event event, Store store) {
            return true;
        }
    }

    static class Action {
        static class Assignment {
            final int storeIndex;
            final int eventIndex;

            Assignment(int storeIndex, int eventIndex) {
                this.storeIndex = storeIndex;
                this.eventIndex = eventIndex;
            }
        }

        HashMap<Integer, Integer> assignments;
        Action(Assignment[] init) {
            assignments = new HashMap<Integer, Integer>();
            for (Assignment a : init) {
//                assert as
                assignments.put(a.storeIndex, a.eventIndex);
            }
        }
    }

    static class TransitionStep {
        final HashSet<Integer> eventIds;
        final Guard guard;
        final Action action;

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
        final TransitionStep[] steps;
        final int target;

        Transition(TransitionStep[] steps, int target) {
            this.steps = steps;
            this.target = target;
        }

        Transition(TransitionStep oneStep, int target) {
            this(new TransitionStep[]{oneStep}, target);
        }
    }

    static class Automaton {
        final int startVertex;
        final int errorVertex;

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

    final private String message;
    final private Automaton automaton;
    private HashSet<State> states;

    public Checker(String message, Automaton automaton) {
        this.message = message;
        this.automaton = automaton;
        this.states = new HashSet<State>();
        states.add(new State(automaton.startVertex));
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
}
/* TODO
    - write some tests
    - fill in the missing methods
    - implement persistent data structures
 */
// vim:sw=4:ts=4:
