package svl.learn.java.java7;

import java.io.Closeable;

public class ClosableWithExceptions {
    public static void main(String[] args) {
        //noinspection OverlyBroadCatchBlock
        try {
            test();
        } catch (Exception e) {
            System.out.println(e.getMessage());
            Throwable[] suppressed = e.getSuppressed();
            for (Throwable throwable : suppressed) {
                System.out.printf("Suppressed: %s\n", throwable.getMessage());
            }
        }
    }

    private static void test() {
        try(
            MyClass o1 = new MyClass();
            MyClass o2 = new MyClass();
            MyClass o3 = new MyClass()
        ){
            System.out.println("o1 = " + o1);
            System.out.println("o2 = " + o2);
            System.out.println("o3 = " + o3);
            System.out.println("Inside the block");
            throw new RuntimeException("From body");
        }
    }

    private static class MyClass implements Closeable {
        @Override
        public void close(){
            System.out.println("Close: " + this);
            throw new RuntimeException("From close");
        }
    }
}