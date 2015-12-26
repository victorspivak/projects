/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.basic;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public class TestOptional {
    public static void main(String[] args) {
        List<Fruit> fruits = Arrays.asList(new Fruit("apple"),
                new Fruit("grape"),
                new Fruit("orange"));

        Optional<Fruit> found = find("lemon", fruits);
        if(found.isPresent()) {
            Fruit fruit = found.get();
            String name = fruit.getName();
            System.out.println("Found: " + name);
        }
        else
            System.out.println("Did not found");

        System.out.println(find("lemon", fruits).map(Fruit::getName).orElse("Oops"));
        System.out.println(find("grape", fruits).map(Fruit::getName).orElse("Oops"));
        System.out.println(find("lemon", fruits).map(Fruit::getName).orElse(null));
        System.out.println(find("lemon", fruits).isPresent());

    }

    public static Optional<Fruit> find(String name, List<Fruit> fruits) {
        for(Fruit fruit : fruits) {
            if(fruit.getName().equals(name)) {
                return Optional.of(fruit);
            }
        }
        return Optional.empty();
    }

    private static class Fruit{
        private String name;

        Fruit(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }
}