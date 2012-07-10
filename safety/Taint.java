interface Str {
  void set(int i, char c);  char get(int i);
  int len();
  Itr itr();
}
interface Itr {
  boolean hasNext();        char next();
  void set(char c); 
}
class CharArray implements Str {
  char [] data;
  // ...
}
class Concat implements Str {
  Str one, two;
  public static Concat make(Str one, Str two) { /* ... */ }
  // ...
}

