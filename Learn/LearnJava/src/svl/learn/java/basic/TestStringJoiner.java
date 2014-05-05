/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.basic;

import java.util.StringJoiner;

public class TestStringJoiner {
    public static void main(String[] args) {
        StringJoiner joiner = new StringJoiner(",");
        joiner.add("name1");
        joiner.add("name2");
        joiner.add("name3");
        System.out.println(joiner.toString());
    }
}