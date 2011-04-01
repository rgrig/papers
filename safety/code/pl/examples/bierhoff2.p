// Some stubs, for things that are not built in the language.
class Byte {}

class ByteArray
  Unit init(Int size)
  Unit set(Int index, Byte data)
  Byte get(Int index)
  var Int length

class Int
  Unit increment()
  Bool lt(Int x)
  Bool le(Int x)
  Byte toByte()

// Figure 7 in "Modular TypeState Checking of Aliased Objects"
class PipedOutputStream
  var PipedInputStream sink

  Unit connect(PipedInputStream snk)
    sink := snk

  Unit write(Int b)
    sink.receive(b)

  Unit close()
    sink.receivedLast()

// Figure 8 in "Modular TypeState Checking of Aliased Objects"
class PipedInputStream
  var Bool closedByWriter
  var Bool closedByReader
  var ByteArray buffer
  var Int in
  var Int out

  Unit init(PipedOutputStream src)
    closedByWriter := * // * was 0
    closedByReader := * // * was 0
    buffer := new
    buffer.init(*)      // * was 1024
    in := *             // * was -1
    out := *            // * was 0
    src.connect(this)

  Unit receive(Int b)
    var Bool c  // condition
    while {} (in == out) {}
    c := in.lt(*) // was 0
    if c
      in := * // was 0
      out := * // was 0
    in.increment()
    var Byte bb := b.toByte()
    buffer.set(in, bb)
    var Int len := buffer.length
    c := len.le(in)
    if c { in := * } // was 0
 
  Unit receivedLast() { closedByWriter := * } // was 1
  Int read()  // says "analogous to receive()", TODO

  Unit close()
    closedByReader := * // was 1
    in := * // was -1

main {}
  // TODO
