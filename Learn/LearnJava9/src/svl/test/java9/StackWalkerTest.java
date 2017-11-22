package svl.test.java9;

import java.util.stream.Stream;

public class StackWalkerTest {
    private StackWalkerTest() {
    }

    public static void main(String[] args) {
        test1();
    }

    private static void test1() {
        StackWalker stackWalker = StackWalker.getInstance();

        test2(stackWalker);
    }

    private static void test2(StackWalker stackWalker) {
        Long walk = stackWalker.walk(Stream::count);

        stackWalker.forEach(sf -> System.out.println("sf = " + sf));

        System.out.println("walk = " + walk);
    }
}