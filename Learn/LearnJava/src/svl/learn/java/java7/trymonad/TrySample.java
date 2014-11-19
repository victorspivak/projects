package svl.learn.java.java7.trymonad;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class TrySample {
    public static void main(String[] args) {
        List<Integer> data1 = Arrays.asList(1, 2, 3, 0, 4, 5);
        List<Try<Integer>> result1 = data1.stream().map(Try.tryOf(i -> 100 / i)).collect(Collectors.toList());
        System.out.println(result1);

        Object result2 = Try.tryOf(() -> data1.stream().map(i -> 100 / i).collect(Collectors.toList()));
        System.out.println(result2);

        List<Integer> data2 = Arrays.asList(1, 2, 3, 4, 5);
        Object result3 = Try.tryOf(() -> data2.stream().map(i -> 100 / i).collect(Collectors.toList()));
        System.out.println(result3);
    }
}