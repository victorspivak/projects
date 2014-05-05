/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.streams;

import java.util.Optional;
import java.util.Random;
import java.util.stream.IntStream;
import java.util.stream.Stream;

@SuppressWarnings({"ClassWithoutConstructor", "UtilityClassWithoutPrivateConstructor", "UtilityClass"})
public class TestStreams {
    public static void main(String[] args) {
        infiniteIntStreamExample();
        findElementInInfiniteStream();


    }

    private static void findElementInInfiniteStream() {
        Random random = new Random();
        Stream<String> stream = Stream.iterate("", str -> "s" + random.nextInt()).skip(1).peek(System.out::println);
//        stream = stream.limit(10);
        Optional<String> found = stream.filter(s -> s.length() < 7).findFirst();

        System.out.println("=======================================");
        System.out.println(found.orElse("Oops"));
        System.out.println("=======================================");
    }

    private static void infiniteIntStreamExample() {
        IntStream.iterate(0, i -> i + 2)
                .limit(10)
                .forEach(System.out::println);
    }
}