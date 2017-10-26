package svl.test.java9;

import java.util.List;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

public class StreamTest {
    public static void main(String[] args) {
        ofNullable();
        whileMethods();
        iterateTest();
    }

    private static void iterateTest() {
        List<Integer> list = Stream.iterate(0, i -> i < 10, i -> i + 1).collect(toList());
        System.out.println("list = " + list);
    }

    private static void whileMethods() {
        System.out.println("takeWhile");
        Stream<Integer> ints = buildIntsStream();
        ints.takeWhile(i -> i < 10).forEach(System.out::println);

        System.out.println("dropWhile");
        ints = buildIntsStream();
        ints.dropWhile(i -> i < 10).forEach(System.out::println);
    }

    private static Stream<Integer> buildIntsStream() {
        return Stream.of(1, 5, 9, 22, 4, 6, 7);
    }

    private static void ofNullable() {
        final String configurationDirectory =
                Stream.of("app.config", "app.home", "user.home")
                        .flatMap(key -> Stream.ofNullable(System.getProperty(key)))
                        .findFirst()
                        .orElseThrow(IllegalStateException::new);
        System.out.println("configurationDirectory = " + configurationDirectory);

        System.out.println("Stream.ofNullable(null) = " + Stream.ofNullable(null).count());
    }
}