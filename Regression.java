/* This file is for regression testing
 * Basically, it should contain every aspects of Java programming language
 * that is covered by the parser
 */

package Hello;

import javax.swing.*;
import java.util.*;
import static java.lang.Math.PI;

interface Moo {
    void foo();
}

interface Hoo {
}

public abstract class Regression<T extends Comparable<T>> extends Object
    implements Moo, Hoo {

    static int a;
    private String name;
    private ArrayList<String> names;

    /* Static initializer */
    static {
        a = 1 + (2 * 3);
    }

    private class Foo {}

    public static void main(String[] args) {
        System.out.println("\"Hello \nWorld\""
                + a
                + 1000.0
                + 0x300);
        int foo = 3;
        final double boo = 3.0;
    }
}
