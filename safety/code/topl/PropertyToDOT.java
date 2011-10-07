package topl;

import java.io.FileWriter;
import static topl.Checker.*;

public class PropertyToDOT {
    public static void main(String[] args) {
	System.out.println("Converting Property.java to DOT...");
	String s = topl.Property.checker.toDOT();
	try {
	    FileWriter f = new FileWriter("Property.dot");
	    f.write(s);
	    f.close();
	} catch(java.io.IOException e) {
	    System.out.println("Error writing to Property.DOT");	    
	}
    }
}