package topl;
public class CheckerTests {
  // from file examples/iter_remove.j
  Checker a = new Checker(
    "trying to advance an iterator on a collection modified by another iterator",
    new Checker.Automaton(0, 1, new Checker.Transition[][]{
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                5},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{
                new Checker.Action.Assignment(2, 0)})),
            new Checker.TransitionStep(
              new int[]{
                4},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{
                new Checker.Action.Assignment(0, 0)}))},
          5)},
      new Checker.Transition[]{},
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                1},
              new Checker.StoreEqualityGuard(0, 0),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                0},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          1)},
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                1},
              new Checker.StoreEqualityGuard(0, 1),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                0},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          1)},
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                3},
              new Checker.StoreEqualityGuard(0, 1),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                2},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          2),
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                3},
              new Checker.StoreEqualityGuard(0, 0),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                2},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          3)},
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                5},
              new Checker.StoreEqualityGuard(0, 2),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                4},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{
                new Checker.Action.Assignment(1, 0)}))},
          4)}}));
  Checker b = new Checker(
    "trying to advance an iterator on a modified collection",
    new Checker.Automaton(0, 1, new Checker.Transition[][]{
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                7},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{
                new Checker.Action.Assignment(1, 0)})),
            new Checker.TransitionStep(
              new int[]{
                6},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{
                new Checker.Action.Assignment(0, 0)}))},
          3)},
      new Checker.Transition[]{},
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                1},
              new Checker.StoreEqualityGuard(0, 0),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                0},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          1)},
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                3},
              new Checker.StoreEqualityGuard(0, 1),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                2},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          2),
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                5},
              new Checker.StoreEqualityGuard(0, 1),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                4},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          2)}}));
  Checker c = new Checker(
    "advancing iterator without checking if not last",
    new Checker.Automaton(0, 1, new Checker.Transition[][]{
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                5},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                4},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{
                new Checker.Action.Assignment(0, 0)}))},
          2)},
      new Checker.Transition[]{},
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                1},
              new Checker.StoreEqualityGuard(0, 0),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                0},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          1),
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                3},
              new Checker.StoreEqualityGuard(0, 0),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                2},
              new Checker.ConstantEqualityGuard(0, 1),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          3)},
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                1},
              new Checker.StoreEqualityGuard(0, 0),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                0},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          2)}}));
  Checker d = new Checker(
    "trying to advance an iterator past the end",
    new Checker.Automaton(0, 1, new Checker.Transition[][]{
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                5},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                4},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{
                new Checker.Action.Assignment(0, 0)}))},
          3)},
      new Checker.Transition[]{},
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                1},
              new Checker.StoreEqualityGuard(0, 0),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                0},
              new Checker.TrueGuard(),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          1)},
      new Checker.Transition[]{
        new Checker.Transition(
          new Checker.TransitionStep[]{
            new Checker.TransitionStep(
              new int[]{
                3},
              new Checker.StoreEqualityGuard(0, 0),
              new Checker.Action(new Checker.Action.Assignment[]{})),
            new Checker.TransitionStep(
              new int[]{
                2},
              new Checker.ConstantEqualityGuard(0, 0),
              new Checker.Action(new Checker.Action.Assignment[]{}))},
          2)}}));
  public static void main(String[] args) {
    System.out.println("TODO");
  }
}
