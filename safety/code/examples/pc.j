property "empty" {}

property "only start"
  start -> start: *.m()

property "only error"
  error -> error: *.m()

property "redundant"
  start -> error: *.m()
  redundant -> redundant: *.m()

property "cycle"
  start -> error: *.m()
  error -> start: *.m()

property "foo"
  start -> a: *.m()
  a -> b: *.m()
  b -> c: *.m()
  b -> d: *.m()
  c -> e: *.m()
  d -> e: *.m()
  e -> f: *.m()
  f -> a: *.m()
  c -> bar: *.m()
  foo -> d: *.m()
  f-> error: *.m()