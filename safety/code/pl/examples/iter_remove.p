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
  unit := new Unit
  var User u := new User
  var Collection c := new Collection
  var Object o := new Object
  c.add(o)
  u.remove(c, o)
