/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.basic;

public class TestInterfaceWithStaticMethod {
    interface IPrint{
        static void print(String str){
            System.out.println(str);
        }
    }
    public static void main(String[] args) {
        IPrint.print("Hello Java 8");
    }
}