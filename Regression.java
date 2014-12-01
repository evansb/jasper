/* This file is for regression testing
 * Basically, it should contain every aspects of Java programming language
 * that is covered by the parser
 */

package Hello;

import javax.swing.*;
import java.util.*;
import static java.lang.Math.PI;

class Foo {

}

public class Regression<T extends Comparable<T>> {
    static int a;
    /* Static initializer */
    static {
        a = 1 + (2 * 3);
    }

    public static void main(String[] args) {
        System.out.println("\"Hello \nWorld\""
                + a
                + 1000.0
                + 0x300);
    }
}
