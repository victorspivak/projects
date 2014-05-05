/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.streams;

import java.util.stream.Stream;

@SuppressWarnings({"AutoUnboxing", "AutoBoxing"})
public class TestReduce {
    public static void main(String[] args) {
        calculateSum();

        System.out.printf("Element count %d\n", StreamsTestHelper.makePersonsAsStream(1000, "name").count());

        System.out.printf("Average age %f\n", StreamsTestHelper.makePersonsAsStream(1000, "name").mapToInt(Person::getAge).average().getAsDouble());
    }

    private static void calculateSum() {
        Stream<Person> persons = StreamsTestHelper.makePersonsAsStream();
        int sum = persons.map(Person::getAge).reduce(0, (a, b) -> a + b);
        System.out.println("sum = " + sum);
    }
}