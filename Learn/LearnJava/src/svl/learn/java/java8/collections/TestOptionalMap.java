package svl.learn.java.java8.collections;

import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.Optional;

import static org.junit.Assert.assertEquals;

/**
 * TestOptionalMap
 *
 * @author victor.spivak
 */
public class TestOptionalMap {
    private OptionalMap<String, Integer> map;

    @Before public void setup() {
        map = OptionalMap.decorate(new HashMap<>());

        map.put("Key1", 1);
        map.put("Key2", 2);
        map.put("Key3", 3);
    }

    @Test public void getOptional() {
        assertEquals(new Integer(1), map.get("Key1"));
        assertEquals(Optional.of(1), map.getOptional("Key1"));
        assertEquals(null, map.get("NonExistingKey"));
        assertEquals(Optional.empty(), map.getOptional("NonExistingKey"));
    }

    @Test public void removeOptional() {
        assertEquals(Optional.of(1), map.removeOptional("Key1"));
        assertEquals(Optional.empty(), map.removeOptional("NonExistingKey"));
    }

    @Test public void replaceOptional() {
        assertEquals(Optional.of(1), map.replaceOptional("Key1", 11));
        assertEquals(Optional.of(11), map.replaceOptional("Key1", 12));
        assertEquals(Optional.empty(), map.replaceOptional("NonExistingKey", 999));
        assertEquals(Optional.empty(), map.replaceOptional("NonExistingKey", 1000));
    }
}
