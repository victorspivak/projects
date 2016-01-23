package svl.learn.java.lambda_in_action;

import java.util.Arrays;
import java.util.stream.Stream;

import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class StreamExamples {
    public static void main(String[] args) {
        System.out.println(Arrays.asList("Hello World".split(" ")).stream()
                .map(w->w.split(""))
                .flatMap(Arrays::stream)
                .distinct()
                .collect(toList()));

        System.out.println("isHealthy = " + DishFactory.makeMenu().allMatch(dish -> dish.getCalories() < 1000));
        System.out.println("isHealthy = " + DishFactory.makeMenu().noneMatch(dish -> dish.getCalories() >= 1000));
        System.out.println("Any Vegi = " + DishFactory.makeMenu().filter(Dish::isVegetarian).findAny());

        System.out.println();
        String fibonacci = Stream.iterate(new int[]{0, 1},
                t -> new int[]{t[1], t[0]+t[1]})
                .map(t->t[1])
                .skip(1)
                .limit(20)
                .map(Object::toString)
                .collect(joining(" "));

        System.out.println("fibonacci = " + fibonacci);
    }
}