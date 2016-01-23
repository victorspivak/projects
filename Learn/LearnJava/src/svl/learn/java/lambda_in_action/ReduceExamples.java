package svl.learn.java.lambda_in_action;

import java.util.Arrays;
import java.util.stream.Stream;

public class ReduceExamples {
    public static void main(String[] args) {
        System.out.println("min = " + getNumbers().reduce(Integer::min));
    }

    private static Stream<Integer> getNumbers() {
        return Stream.of(1,2,3,4,5,6,7,8,9,10);
    }
}