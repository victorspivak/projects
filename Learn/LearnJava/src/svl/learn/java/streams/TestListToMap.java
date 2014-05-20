/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.streams;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TestListToMap {
    public static void main(String[] args) {
        Stream<Person> persons = StreamsTestHelper.makePersonsAsStream();

        Map<String, Person> map = persons.collect(
                Collectors.toMap(Person::getName, (p) -> p));

        System.out.println("map = " + map);
    }
}