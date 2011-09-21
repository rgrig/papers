public class HiyaInstr {
    public static void main(String[] args) {
	greet("World");
    }

    public static void greet(String s) {
	Object[] values = new Object[1];
	values[0] = s;
	check(606176293, values);
	System.out.println("Hiya "+s+"!");
    }
	
    public static void check(int event_id, Object[] events) {
	System.out.println("Checked "+event_id+".");
    }
}

class HE extends HiyaInstr {
    public static void greet(String s) {
	System.out.println("Hiya Extended "+s+"!");
    }
}

class HEE extends HE {
    public static void check(int event_id, Object[] events, int extra) {
	System.out.println("HEE!");
    }
}