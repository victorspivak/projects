package svl.learn.java.basic;

/**
 * TestClassForName
 *
 * @author victor.spivak
 * @since 196
 */
public class TestClassForName {
    public static class Foo extends TestClassForName {
    }

    public static void main(String[] args) {
        try {
            final Class<?> aClass = Class.forName("svl.learn.java.basic.TestClassForName$Foo");
            System.out.println("aClass = " + aClass);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }
}
