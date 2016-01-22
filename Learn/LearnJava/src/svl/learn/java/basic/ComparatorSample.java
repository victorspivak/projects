package svl.learn.java.basic;

import java.util.Arrays;
import java.util.Comparator;

public class ComparatorSample {
    public static void main(String[] args) {
        String[] strings = "I love Scala and Java and I hate PHP".split(" ");
        sort1(strings);
        sort2(strings);
        sort3(strings);
    }

    private static void sort1(String[] strings) {
        Arrays.stream(strings).sorted(Comparator.comparing(String::length)).
                distinct().
                forEach(s -> System.out.print(s + " "));
        System.out.println();
    }

    private static void sort2(String[] strings) {
        Arrays.stream(strings).sorted(Comparator.comparing(String::length).reversed()).
                distinct().
                forEach(s -> System.out.print(s + " "));
        System.out.println();
    }

    private static void sort3(String[] strings) {
        Arrays.stream(strings).sorted(Comparator.comparing(String::length).thenComparing(String::toString)).
                distinct().
                forEach(s -> System.out.print(s + " "));
        System.out.println();
    }
}