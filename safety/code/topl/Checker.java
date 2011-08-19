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
        public int vertex;
        public HashMap<Integer, Object> stack;
        public ArrayDeque<Event> events;
        // The state {this} arose from {previous_state} when {arrival_event} was seen.
        public State previous_state;
        public Event arrival_event;

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
        public HashSet<Integer> event_ids;
        Guard guard;
        Action action;

        boolean evaluateGuard(Event event) {
            // TODO
            return false;
        }
        // TODO: applyAction consumes one event
    }

    static class Transition {
        public TransitionStep[] steps;
        public int target;
    }

    static class Automaton {
        int start_vertex;
        int error_vertex;
        // {transition[vertex]} is outgoing edges.
        public ArrayDeque<Transition>[] transitions;
            // TODO: rename to {outgoingArcsOf}?

        private int maximumTransitionDepth = -1;

        int maximumTransitionDepth() {
            if (maximumTransitionDepth == -1) {
                maximumTransitionDepth = 0;
                for (ArrayDeque<Transition> ts : transitions) {
                    for (Transition t : ts) {
                        maximumTransitionDepth =
                            Math.max(maximumTransitionDepth, t.steps.length);
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
        HashSet<State> departed_states = new HashSet<State>();
        HashSet<State> arrived_states = new HashSet<State>();
        for (State state : states) {
            state.events.addLast(event);
            if (state.events.size() < automaton.maximumTransitionDepth()) {
                continue;
            }
            for (Transition transition : automaton.transitions[state.vertex]) {
                // evaluate transition
                State step_state = state;
                int i = 0;
                Iterator<Event> j = state.events.iterator();
                for (; i < transition.steps.length; ++i) {
                    TransitionStep step = transition.steps[i];
                    Event step_event = j.next();
                    // if (!step.event_ids.contains(step_event.id)) break;
                    if (!step.evaluateGuard(step_event)) break;
                    step_state = step_state.applyAction(step.action);
                }
                // record transition
                if (i == transition.steps.length) {
                    departed_states.add(state);
                    arrived_states.add(step_state);
                    // check for error state
                    if (transition.target == automaton.error_vertex)
                        // TODO
                        reportError();
                }
            }
            // perform transitions
            states.removeAll(departed_states);
            states.addAll(arrived_states);
        }
    }
}
/* TODO
    - rename edge -> arc
    - remove (most) {public} qualifiers
 */
// vim:sw=4:ts=4:
