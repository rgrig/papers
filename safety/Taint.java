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
       
    ....
    ....      
}

class Concat implements Str {
  Str one, two;
  int len;

   .....
   .....

  public static Concat make(Str one, Str two) { return new Concat(one, two);}
}

