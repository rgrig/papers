package examples;

// Local version of IterExample.java
// in that it does not require instrumentation of library code

// dummy iterator
class Iter<E> {
    public E next() {
	return null;
    }
    public boolean hasNext() {
	return false;
    }
    public void remove() {}
}

// dummy collection
class Collector<E> {
    public Iter<E> iterator() {
	return new Iter<E>();
    }
    public void add(E e) {}
}

class LocalUser<E> {
    void remove(Collector<E> c, E x) {
	Iter<E> i = c.iterator();
	while (i.hasNext()) {
	    E e = i.next();
	    if (e.equals(x)) i.remove();
	}
    }
}

public class LocalIterExample {
    public static void main(String[] args) {
	LocalUser<Integer> u = new LocalUser<Integer>();
	Collector<Integer> c = new Collector<Integer>();
	Integer i = new Integer(3);
	c.add(i);
	Iter<Integer> it = c.iterator();
	u.remove(c, i); // changes c, so it becomes invalid
	i = it.next(); // should result in error from automaton
    }
}