package svl.learn.java.lambda_in_action;

import static java.util.Comparator.comparingInt;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.maxBy;
import static java.util.stream.Collectors.toList;

public class CollectExamples {
    public static void main(String[] args) {
        System.out.println(DishFactory.makeMenu().collect(groupingBy(Dish::getType)));
        System.out.println(DishFactory.makeMenu().map(Dish::getName).collect(toList()));
        System.out.println();
        System.out.println("mostCalorieDish = " + DishFactory.makeMenu().collect(maxBy(comparingInt(Dish::getCalories))));

    }
}