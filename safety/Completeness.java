import java.util.*;
public class Completeness {
   Collection c;
   Random r=new Random();
   public void NeedsUnboundMemory(int n) {
      Iterator[] a = new Iterator[n];
      for (int i=0; i<n; i++) {
          a[i]=c.iterator();
          if (r.nextBoolean()) 
	    while (a[i].hasNext()) a[i].next();	
      };
      a[r.nextInt(n)].next();
  };
}
