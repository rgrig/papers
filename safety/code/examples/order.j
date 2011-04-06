//  Inspired by
//    org.apache.tools.ant.filters.ConcatFilter
//  You can concatenate files by making a list of streams.

class Byte {}

class Stream
  Byte read()
  Bool hasMore()

class ConcatStream  // TODO: Should really inherit from Stream
  var Stream first
  var Stream second

  Unit init(Stream a, Stream b)
    first := a
    second := b

  // The property to check is that first.hasMore() returned true
  // before second.read() is called.
  Byte read()
    var Bool f := first.hasMore()
    var Byte r
    if f
      r := first.read()
    else
      r := second.read()
    return r

main
  var Stream a := new
  var Stream b := new
  var ConcatStream c := new
  c.init(a, b)
  while *
    c.read()
