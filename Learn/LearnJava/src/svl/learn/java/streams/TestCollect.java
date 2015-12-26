package svl.learn.java.streams;

import java.util.HashMap;
import java.util.Map;
import java.util.function.IntConsumer;

/**
 * TestCollect
 *
 * @author victor.spivak
 * @since 200
 */
public class TestCollect {
    public static void main(String[] args) {
        Averager averageCollect = StreamsTestHelper.makePersonsAsStream()
                .filter(p -> p.getGender() == Person.Sex.MALE)
                .map(Person::getAge)
                .collect(Averager::new, Averager::accept, Averager::combine);

        System.out.println("Average age of male members: " + averageCollect.average());

        Map<String, Person> map = StreamsTestHelper.makePersonsAsStream()
                .collect(HashMap::new, (m, p) -> m.put(p.getName(), p), HashMap::putAll);

        System.out.println("map = " + map);
    }

    static class Averager implements IntConsumer
    {
        private int total = 0;
        private int count = 0;

        public double average() {
            return count > 0 ? ((double) total)/count : 0;
        }

        public void accept(int i) { total += i; count++; }

        public void combine(Averager other) {
            total += other.total;
            count += other.count;
        }
    }
}
