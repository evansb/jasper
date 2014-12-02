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

interface Hoo extends Moo, Comparable<Moo> {
    interface WeNeedToGoDeeper {
    }
}

enum Demo {
    HELLO, GOODBYE
}

public abstract class Regression<T extends Comparable<T>> extends Object
    implements Moo, Hoo {

    static int a;
    static boolean b;
    private String name;
    private ArrayList<String> names;
    private volatile String vola;

    /* Static initializer */
    static {
        a = 1 + (2 * 3) & 4 | 32;
        b = true && false || true;
        b = 3 <= 4;
    }

    private class Foo {}

    public Regression() {
        super();
        name = "Hello World";
    }

    private void sayHello() {
        this.sayHello();
    }

    private void sayGoodbye(
            ArrayList<? extends Object> hello,
            ArrayList<? super Object> hello2) {
    }

    public static void main(String[] args) {
        System.out.println("\"Hello \nWorld\""
                + a
                + 1000.0
                + 0x300);
        int foo = 3;
        final double boo = 3.0;
        while (true) {
            Integer[] coo = new Integer[20];
            int[][][] loo;
            double[] soo = { 1.0, 2.0, 3.0 };
            ArrayList<Integer> as = new ArrayList<Integer>();
            as.add((new String()).toString().length());
            for (int a : coo) {
                continue;
            }
            for (int x = 4, y = 3; y <= x; y++)  {
                do {
                    x += 3;
                } while (x < 1);
            }
            switch (foo) {
                case 3 :
                    break;
                case 2 :
                case 1 :
                    foo *= 3;
                    break;
                default:
                    break;
            }
label:
            foo--;
            new String("Hello World").length();
            /* Java 8 stuff */
            new Thread(Regression::doWork).start();
            new Thread(() -> doWork()).start();
            new Thread(Object::new).start();
        }
    }

    static void doWork() {
        try {
            throw new InterruptedException();
        }
        catch (InterruptedException ie){
        }
    }

    public synchronized void enterCS() {
        synchronized(this) {

        }
    }
}
