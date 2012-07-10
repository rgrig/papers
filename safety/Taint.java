interface Str {
  void set(int i, char c);
  char get(int i);
  int len();
  Itr itr();
}

interface Itr {
  char next();
  void set(char c); 
  boolean hasNext();
}

class CharArray implements Str {
  char [] data;
  public CharArray(int len)                 { data = new char[len]; }

  @Override public CaItr itr()              { return new CaItr(); }
      ....

  class CaItr implements Itr {
    int i;
    public CaItr() { i = -1; }
    @Override public char next()        { return data[++i]; }
    @Override public void set(char c)   { data[i] = c; }
    @Override public boolean hasNext()  { return i + 1 < data.length; }
  }
}

class Concat implements Str {
  Str one, two;
  int len;
  private Concat(Str one, Str two) {
    this.one = one; this.two = two; len = one.len() + two.len();
  }
  public static Concat make(Str one, Str two) { return new Concat(one, two);}

  @Override public CoItr itr() { return new CoItr(); }
     .....

  class CoItr implements Itr {
    boolean first;
    Itr i;
    public CoItr() { i = one.itr(); first = true; }
    @Override public char next() { maybeSwap(); return i.next(); }
    @Override public void set(char c) { i.set(c); }
    @Override public boolean hasNext() { maybeSwap(); return i.hasNext(); }
    private void maybeSwap() {
      if (first && !i.hasNext()) { i = two.itr(); first = false; }
    }
  }
}

