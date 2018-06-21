/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.streams;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toMap;

@SuppressWarnings({"AutoUnboxing", "AutoBoxing"})
public class TestReduce {
    public static void main(String[] args) {
        calculateSum();

        System.out.printf("Element count %d\n", StreamsTestHelper.makePersonsAsStream(1000, "name").count());

        System.out.printf("Average age %f\n", StreamsTestHelper.makePersonsAsStream(1000, "name").mapToInt(Person::getAge).average().getAsDouble());

        Map<String, String> map1 = Stream.of(
                new AbstractMap.SimpleEntry<>("A", "a"),
                new AbstractMap.SimpleEntry<>("B", "b"),
                new AbstractMap.SimpleEntry<>("C", "c"),
                new AbstractMap.SimpleEntry<>("Z", "z"))
                .collect(toMap(AbstractMap.SimpleEntry::getKey, AbstractMap.SimpleEntry::getValue));
        Map<String, String> map2 = Stream.of(
                new AbstractMap.SimpleEntry<>("D", "d"),
                new AbstractMap.SimpleEntry<>("E", "e"),
                new AbstractMap.SimpleEntry<>("Y", "y"))
                .collect(toMap(AbstractMap.SimpleEntry::getKey, AbstractMap.SimpleEntry::getValue));
        Map<String, String> map3 = Stream.of(
                new AbstractMap.SimpleEntry<>("K", "k"),
                new AbstractMap.SimpleEntry<>("L", "l"),
                new AbstractMap.SimpleEntry<>("X", "x"))
                .collect(toMap(AbstractMap.SimpleEntry::getKey, AbstractMap.SimpleEntry::getValue));

        Map<String, String> map = Stream.of(map1, map2, map3)
                .reduce(new HashMap<>(), (r, m) -> {r.putAll(m); return r;});

        System.out.println("map = " + map);
    }

    private static void calculateSum() {
        Stream<Person> persons = StreamsTestHelper.makePersonsAsStream();
        int sum = persons.map(Person::getAge).reduce(0, (a, b) -> a + b);
        System.out.println("sum = " + sum);
    }
}