// ee: package declaration
package application.helloworld;
// ee: package declaration
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * HelloWorld.java
 */

public class HelloWorld
{
    // be: specific constructor
    public HelloWorld()
    {
        System.out.println("Object HelloWorld created");
    } // end of specific constructor "HelloWorld()"
    // ee: specific constructor

    public static final void main(final String[] args)
    {
        String baseName = "HelloWorld";
        ResourceBundle rb = ResourceBundle.getBundle(baseName);
        String greetings = rb.getString("hello_msg");
        System.out.printf("%s\n", greetings);
    } // end of method "main(String[] args)"
} // end of class "HelloWorld"

// Auto-complete
// See http://stackoverflow.com/questions/239732/things-possible-in-intellij-that-arent-possible-in-eclipse

interface Person {
    String getName();
    String getAddress();
    int getAge();
}
//---
Person p;
// String name = p.<CTRL-SHIFT-SPACE>
String name = p.

// and it shows you ONLY getName(), getAddress() and toString() (only they are
// compatible by type) and getName() is first in the list because it has more
// relevant name.
