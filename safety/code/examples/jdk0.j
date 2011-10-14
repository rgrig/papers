property IteratorComodification
  observing <java.util.{Iterator.{remove,next},Collection.iterator}>
  prefix <java.util.{Iterator,Collection}>
  start -> gotOne:    I := C.iterator()
  gotOne -> gotTwo:   J := c.iterator()
  gotOne -> gotOne:   *
  gotTwo -> jInvalid: i.remove()
  gotTwo -> iInvalid: j.remove()
  jInvalid -> error:  j.next()
  iInvalid -> error:  i.next()
