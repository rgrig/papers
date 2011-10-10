package topl;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;

public class Checker {
    /*
        Some classes have a method {check()} that asserts if some object
        invariant is broken. These functions always return {true} so that
        you can say {assert x.check()} in case you want to skip them
        completely when assertions are not enabled.
     */

    static class Treap<T extends Comparable<T>> {
        static final private Random random = new Random(123);

        final int priority;
        final T data;
        final Treap<T> left;
        final Treap<T> right;

        Treap() {
            priority = 0;
            data = null;
            left = right = null;
            check();
        }

        private Treap(int priority, T data, Treap<T> left, Treap<T> right) {
            this.priority = priority;
            this.data = data;
            this.left = left;
            this.right = right;
            // Invariant {check()} may be broken!
        }

        boolean check() {
            assert check(null, null, Integer.MAX_VALUE);
            return true;
        }

        boolean check(T minimum, T maximum, int high) {
            assert high > 0;
            if (data == null) {
                assert left == null;
                assert right == null;
                assert priority == 0;
            } else {
                assert left != null;
                assert right != null;
                assert priority > 0;
                assert priority <= high;
                if (minimum != null) {
                    assert minimum.compareTo(data) <= 0;
                }
                if (maximum != null) {
                    assert data.compareTo(maximum) <= 0;
                }
                assert left.check(minimum, data, priority);
                assert right.check(data, maximum, priority);
            }
            return true;
        }

        private Treap<T> rotateLeft() {
            assert data != null;
            return new Treap<T>(
                    right.priority, right.data,
                    new Treap<T>(priority, data, left, right.left),
                    right.right);
        }

        private Treap<T> rotateRight() {
            assert data != null;
            return new Treap<T>(
                    left.priority, left.data,
                    left.left,
                    new Treap<T>(priority, data, left.right, right));
        }

        private Treap<T> balance() {
            assert data != null;
            assert left.priority <= priority || right.priority <= priority;
            Treap<T> result = this;
            if (left.priority > priority) {
                result = result.rotateRight();
            } else if (right.priority > priority) {
                result = result.rotateLeft();
            }
            assert result.check();
            return result;
        }

        private Treap<T> insert(int newPriority, T newData) {
            assert newData != null;
            assert newPriority > 0;
            if (data == null) {
                return new Treap<T>(newPriority, newData, this, this);
            } else {
                int c = newData.compareTo(data);
                if (c < 0) {
                    return new Treap<T>(priority, data,
                            left.insert(newPriority, newData),
                            right)
                            .balance();
                } else if (c > 0) {
                    return new Treap<T>(priority, data,
                            left,
                            right.insert(newPriority, newData))
                            .balance();
                } else {
                    return this;
                }
            }
        }

        Treap<T> insert(T data) {
            return insert(random.nextInt(Integer.MAX_VALUE - 1) + 1, data);
        }

        static boolean priorityLess(int p, int q) {
            return p < q || (p == q && random.nextBoolean());
        }

        Treap<T> remove(T oldData) {
            Treap<T> result = this;
            if (data != null) {
                int c = oldData.compareTo(data);
                if (c < 0) {
                    result = new Treap<T>(priority, data,
                            left.remove(oldData),
                            right);
                } else if (c > 0) {
                    result = new Treap<T>(priority, data,
                            left,
                            right.remove(oldData));
                } else {
                    if (left.data == null && right.data == null) {
                        return left;
                    } else if (left.data == null) {
                        return right.remove(oldData);
                    } else if (right.data == null) {
                        return left.remove(oldData);
                    } else if (priorityLess(left.priority, right.priority)) {
                        result = rotateLeft();
                        result = new Treap<T>(result.priority, result.data,
                                left.remove(oldData),
                                right);
                    } else {
                        result = rotateRight();
                        result = new Treap<T>(result.priority, result.data,
                                left,
                                right.remove(oldData));
                    }
                }
            }
            assert result.check();
            return result;
        }

        public T get(T x) {
            assert x != null;
            if (data == null) {
                return null;
            } else {
                int c = x.compareTo(data);
                if (c < 0) {
                    return left.get(x);
                } else if (c > 0) {
                    return right.get(x);
                } else {
                    return data;
                }
            }
        }
    }

    // TODO(rgrig): Might want to produce a set with a faster {contains}
    static Set<Integer> setOf(int[] xs) {
        HashSet<Integer> r = new HashSet<Integer>();
        for (int x : xs) {
            r.add(x);
        }
        return r;
    }

    public static class Event {
        final int id;
        final Object[] values;

        public Event(int id, Object[] values) {
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
        static class Binding implements Comparable<Binding> {
            final int variable;
            final Object value;

            public Binding(int variable, Object value) {
                this.variable = variable;
                this.value = value;
            }

            @Override
            public int compareTo(Binding other) {
                if (variable < other.variable) {
                    return -1;
                } else if (variable > other.variable) {
                    return +1;
                } else {
                    return 0;
                }
            }
        }

        // TODO: Hashing
        final int vertex;
        final Treap<Binding> stack;
        final ArrayDeque<Event> events;
        // The state {this} arose from {previousState} when {arrivalEvent} was seen.
        final State previousState;
        final Event arrivalEvent;

        State(int vertex) {
            this.vertex = vertex;
            this.stack = new Treap<Binding>();
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

	@Override
	public String toString() {
	    if (children.length == 0) return "*";
	    if (children.length == 1) return children[0].toString();
	    StringBuffer s = new StringBuffer();
	    s.append("and (");
	    for (Guard g : children) {
		s.append(g);
		s.append(", ");
            }
	    s.delete(s.length()-2, s.length());
	    s.append(")");
	    return s.toString();
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

	@Override
	public String toString() {
	    return "not (" + child + ")";
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

	@Override
	public String toString() {
	    return "event[" + eventIndex + "] == store[" + storeIndex + "]";
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

	@Override
	public String toString() {
	    return value + " == event[" + eventIndex + "]";
	}
    }

    static class TrueGuard implements Guard {
        @Override
        public boolean evaluate(Event event, Store store) {
            return true;
        }

	@Override
	public String toString() {
	    return "*";
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
        final Set<Integer> eventIds;
        final Guard guard;
        final Action action;

        TransitionStep(int[] eventIds, Guard guard, Action action) {
            this.eventIds = setOf(eventIds);
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
        private static class VertexEvent {
            int vertex;
            int eventId;
            public VertexEvent(int vertex, int eventId) {
                this.vertex = vertex;
                this.eventId = eventId;
            }
	    @Override
            public boolean equals(Object o) {
		if (o instanceof VertexEvent) {
		    VertexEvent ve = (VertexEvent)o;
		    return (vertex == ve.vertex && eventId == ve.eventId);
		}
		else return false;
	    }
	    @Override
	    public int hashCode() {
		return 31*vertex + 101*eventId;
	    }
        }
        private HashSet<VertexEvent> interesting = new HashSet<VertexEvent>();

        final int[] startVertices;
        final String[] errorMessages;

        final Transition[][] transitions;
            // {transitions[vertex]}  are the outgoing transitions of {vertex}

        private int maximumTransitionDepth = -1;

        Automaton(int[] startVertices, String[] errorMessages,
                Transition[][] transitions, int[] filterOfState,
                int[][] filters) {
            this.startVertices = startVertices;
            this.errorMessages = errorMessages;
            this.transitions = transitions;
            for (int s = 0; s < transitions.length; ++s) {
                for (int e : filters[filterOfState[s]]) {
                    interesting.add(new VertexEvent(s, e));
                }
            }
            assert check();
        }

        boolean check() {
            assert transitions != null;
            assert errorMessages.length == transitions.length;
            for (int v : startVertices) {
                assert 0 <= v && v < transitions.length;
                assert errorMessages[v] == null;
            }
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

        boolean isInteresting(int eventId, int vertex) {
            return interesting.contains(new VertexEvent(vertex, eventId));
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

    final private Automaton automaton;
    private HashSet<State> states;

    public Checker(Automaton automaton) {
        this.automaton = automaton;
        this.states = new HashSet<State>();
        for (int v : automaton.startVertices)
            states.add(new State(v));
    }

    void reportError(String msg) {
        // TODO
	System.out.println(msg);
    }

    public void check(Event event) {
	System.out.print("Received event id " + event.id + ". States: [");
	for (State state : states) System.out.print(state.vertex + ": " + state.events.size() + ", ");
	System.out.println("]");
        HashSet<State> departedStates = new HashSet<State>();
        HashSet<State> arrivedStates = new HashSet<State>();
        for (State state : states) {
            if (!automaton.isInteresting(event.id, state.vertex)) {
		continue;
            }
            state.events.addLast(event);
            if (state.events.size() < automaton.maximumTransitionDepth()) {
                continue;
            }
	    boolean anyEnabled = false;
            for (Transition transition : automaton.transitions[state.vertex]) {
                // evaluate transition
                State stepState = state;
                int i = 0;
                Iterator<Event> j = state.events.iterator();
		//System.out.print("  stream: <");
                for (; i < transition.steps.length; ++i) {
                    TransitionStep step = transition.steps[i];
                    Event stepEvent = j.next();
		    //System.out.print("  " + stepEvent.id);
                    // if (!step.eventIds.contains(stepEvent.id)) break;
                    if (!step.evaluateGuard(stepEvent)) break;
		    System.out.print(", ");
                    stepState = stepState.applyAction(step.action);
                }
		//System.out.println(">");
                // record transition
                if (i == transition.steps.length) {
		    anyEnabled = true;
                    departedStates.add(state);
                    arrivedStates.add(stepState);
                    // check for error state
                    String msg = automaton.errorMessages[transition.target];
                    if (msg != null)
                        reportError(msg);
                }
            }
	    if (!anyEnabled) // drop an event from the state's queue
		state.events.removeFirst();
        }
	// perform transitions
	states.removeAll(departedStates);
	states.addAll(arrivedStates);
    }

    public String toDOT() {
	StringBuffer s = new StringBuffer();
	s.append("digraph Property {\n");
	// add states as circles
	for (int i = 0; i < automaton.transitions.length; i++) {
	    s.append("  S_");
	    s.append(i);
	    s.append(" [label=\"");
	    s.append(i);
	    if (automaton.errorMessages[i] != null) {
		s.append(" : ");
		s.append(automaton.errorMessages[i]);
		s.append("\", shape=box];\n");
	    }
	    else s.append("\", shape=circle];\n");
	}
	// make start states double circles
	for (int i : automaton.startVertices) {
	    s.append("  S_");
	    s.append(i);
	    s.append(" [shape=doublecircle];\n");
	}
	// add transitions
	for (int i = 0; i < automaton.transitions.length; i++)
	    for (Transition transition : automaton.transitions[i]) {
		s.append("  S_");
		s.append(i);
		s.append(" -> S_");
		s.append(transition.target);
		s.append(" [label=\"");
		for (TransitionStep step : transition.steps) {
		    s.append(step.eventIds.toString());
		    s.append(step.guard.toString());
		    s.append("<");
		    for(Map.Entry a : step.action.assignments.entrySet()) {
			s.append(a.getKey());
			s.append(" <- ");
			s.append(a.getValue());
			s.append(", ");
		    }
		    if (step.action.assignments.size() > 0) s.delete(s.length()-2, s.length());
		    s.append(">; ");
		}
		if (transition.steps.length > 0) s.delete(s.length()-2, s.length());
		s.append("\"];\n");
	    }
	s.append("}\n");
	return s.toString();
    }

    public static void main(String[] args) {
        //CheckerTests t = new CheckerTests();
        //t.c.check(new Event(5, new Object[]{}));
        //t.c.check(new Event(4, new Object[]{}));
        //t.c.check(new Event(1, new Object[]{}));
        //t.c.check(new Event(0, new Object[]{}));
    }
}
/* TODO
    - implement persistent data structures
    - fill in the missing methods
    - write some tests
    - make sure that Checker does *not* call itself recursively when it uses
      the Java API. (Use a flag. And minimize external dependencies (including
      Java API.)
    - report traces when something goes wrong
 */
// vim:sw=4:ts=4:
