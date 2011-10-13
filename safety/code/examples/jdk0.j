property IteratorComodification
  observing <java.util.{Iterator,Collection}.*>
  prefix <java.util.{Iterator,Collection}>
  start -> gotOne:    I := C.iterator()
  gotOne -> gotTwo:   J := c.iterator()
  gotTwo -> jInvalid: i.remove()
  gotTwo -> iInvalid: j.remove()
  jInvalid -> error:  j.next()
  iInvalid -> error:  i.next()