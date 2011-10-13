property OtherModified
  message "trying to advance an iterator on a collection modified by another iterator"
  prefix <java.util.*>
  observing <java.util.Collection.*>
  start -> gotOne:    I := C.iterator()
  gotOne -> gotTwo:   J := c.iterator()
  gotTwo -> jInvalid: i.remove()
  gotTwo -> iInvalid: j.remove()
  jInvalid -> error:  j.next()
  iInvalid -> error:  i.next()
