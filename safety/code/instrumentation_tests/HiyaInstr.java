public class HiyaInstr {
    public static void main(String[] args) {
	System.out.println("main : ");
	greet("World");
    }

    public static void greet(String s) {
	System.out.println("greet : ");
	System.out.println("Hiya "+s+"!");
    }
}