package svl.learn.java.java7;

import java.io.Closeable;

public class ClosableExample {
    public static void main(String[] args) {
        try(
                MyClass o1 = new MyClass();
                MyClass o2 = new MyClass();
                MyClass o3 = new MyClass();
        ){
            System.out.println("o1 = " + o1);
            System.out.println("o2 = " + o2);
            System.out.println("o3 = " + o3);
            System.out.println("Inside the block");
        }
    }

    private static class MyClass implements Closeable{
        @Override
        public void close(){
            System.out.println("Close: " + this);
        }
    }
}