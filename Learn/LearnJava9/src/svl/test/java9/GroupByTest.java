package svl.test.java9;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.*;

public class GroupByTest {
    static class IdId {
        private String id1;
        private String id2;

        public IdId(String id1, String id2) {
            this.id1 = id1;
            this.id2 = id2;
        }

        public String getId1() {
            return id1;
        }

        public String getId2() {
            return id2;
        }

        @Override
        public String toString() {
            return "IdId{" +
                    "id1='" + id1 + '\'' +
                    ", id2='" + id2 + '\'' +
                    '}';
        }
    }

    public static void main(String[] args) {
        List<IdId> vals = List.of(new IdId("1", "v1"), new IdId("2", "v2"), new IdId("1", "v3"), new IdId("2", "v4"), new IdId("1", "v5"));

        Map<String, List<String>> groups = vals.stream().collect(groupingBy(IdId::getId1, mapping(IdId::getId2, toList())));
        System.out.println(groups);
    }
}
