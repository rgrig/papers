import java.util.*;
public class Cme {
  public static void main(String[] args) {
    List<Integer> c = new ArrayList<Integer>();
    c.add(1); c.add(2);
    Iterator<Integer> i = c.iterator();
    Iterator<Integer> j = c.iterator();
    i.next(); i.remove(); j.next();
  }
}
