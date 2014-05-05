/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.streams;

import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.stream.Stream;

public class StreamsTestHelper {
    public static List<Person> makePersons(){
        return Arrays.asList(
            new Person("Male 1", 22, Person.Sex.MALE),
            new Person("Male 2", 32, Person.Sex.MALE),
            new Person("Male 3", 42, Person.Sex.MALE),
            new Person("Male 4", 62, Person.Sex.MALE),
            new Person("Male 5", 12, Person.Sex.MALE),
            new Person("Female 1", 44, Person.Sex.FEMALE),
            new Person("Female 2", 24, Person.Sex.FEMALE),
            new Person("Female 3", 34, Person.Sex.FEMALE),
            new Person("Female 4", 64, Person.Sex.FEMALE),
            new Person("Female 5", 14, Person.Sex.FEMALE)
        );
    }

    public static Stream<Person> makePersonsAsStream(){
        return makePersons().stream();
    }

    public static Stream<Person> makePersonsAsStream(int count, String namePrefix){
        PersonBuilder personBuilder = new PersonBuilder(namePrefix);

        return Stream.generate(personBuilder::makeRandomPerson).limit(count);
    }

    static class PersonBuilder{
        private Random random = new Random();
        private String namePrefix;

        PersonBuilder(String namePrefix) {
            this.namePrefix = namePrefix;
        }

        public Person makeRandomPerson(){
            String name = namePrefix + random.nextInt();
            int age = random.nextInt(100) + 1;
            Person.Sex gender = random.nextBoolean() ? Person.Sex.MALE : Person.Sex.FEMALE;

            return new Person(name, age, gender);
        }
    }
}