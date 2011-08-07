package topl;

public class Checker {
    static class Event {
	int id;
	Object[] values;
    }

    public class State {
	// TODO: Hashing
	public int vertex;
	public HashMap<Integer, Object> stack;
	public ArrayDeque<Event> event_queue;
	// The state {this} arose from {previous_state} when {arrival_event} was seen.
	public State previous_state;
	public Event arrival_event;
    }

    static class TransitionStep {
	// TODO: Consider bitmaps
	public HashSet<Integer> event_ids;
	// TODO: evaluateGuard
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
    }

    private Automaton automaton;
    private HashSet<State> states;

    private int max_trans_depth;

    public Checker(Automaton automaton, int state) {
	this.automaton = automaton;
	this.states = new HashSet<State>();
	states.add(state);
	max_trans_depth = compute_that();
    }

    public void check(Event event) {
	HashSet<State> departed_targets = new HashSet<State>();
	HashSet<State> arrived_targets = new HashSet<State>();
	for(State state : states) {
	    state.event_queue.addLast(event);
	    if(state.event_queue.size() >= max_trans_depth) {
		//ArrayDeque<Transition> transitions = automaton.transitions[state];
		for (Transition transition : automaton.transitions[state.vertex]) {
		    // evaluate transition
		    State step_state = state;
		    int i = 0;
		    for (; i < transition.steps.length; ++i) {
			TransitionStep step = transition.steps[i];
			Event step_event = state.event_queue.get(i);
			// if (!step.event_ids.contains(step_event.id)) break;
			if (!step.evaluateGuard(step_event)) break;
			step_state = step_state.applyAction(step.action);
		    }
		    // record transition
		    if (i == transition.steps.length) {
			departed_targets.add(state);
			arrived_targets.add(step_state);
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
}