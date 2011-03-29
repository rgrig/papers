class Object {}

class Iterator
  Bool hasNext()
  Object next()
  Unit remove()

class Collection
  Iterator iterator()
  Unit add(Object o)

var Unit unit

class User
  Unit remove(Collection c, Object x)
    var Bool hasNext
    var Iterator i := c.iterator()
    while { hasNext := i.hasNext() } (hasNext)
      var Object y := i.next()
      if x == y { i.remove() }
    return unit

main
  unit := new
  var User u := new
  var Collection c := new
  var Object o := new
  c.add(o)
  u.remove(c, o)
