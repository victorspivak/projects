package svl.test.java9;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static java.util.Map.entry;

public class CollectionFactoryTest {
    private CollectionFactoryTest() {
    }

    public static void main(String[] args) {
        List<String> list = List.of("a", "c", "z", "x");
        Set<String> set = Set.of("a", "c", "z", "x");
        Map<String, String> map1 = Map.of("USA", "Vashington", "Russia", "Moskow");
        Map<String, String> map2 = Map.ofEntries(
                entry("USA", "Vashington"),
                entry("Russia", "Moskow"));

        System.out.println("list = " + list);
        System.out.println("set = " + set);
        System.out.println("map1 = " + map1);
        System.out.println("map2 = " + map2);
    }
}