package examples;

import java.util.*;
public class IncorrectIteratorUse {
  public static void main(String[] args) {
    topl.Property.checker.checkerEnabled = true;
    List<Integer> c = new ArrayList<Integer>();
    c.add(1); c.add(2);
System.out.println("l1");
    Iterator<Integer> i = c.iterator();
System.out.println("l2");
    Iterator<Integer> j = c.iterator();
System.out.println("l3");
    i.next();
System.out.println("l4");
    i.remove();
System.out.println("l5");
    j.next();
System.out.println("l6");
  }
}
