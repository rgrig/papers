property Null
  message "go() called on null"
  prefix <examples.*>
  observing <examples.*>
  start -> start: call *.go()
  start -> start: return *.go()
  start -> start: return *
  start -> start: call *
  start -> error: call <null>.go()

