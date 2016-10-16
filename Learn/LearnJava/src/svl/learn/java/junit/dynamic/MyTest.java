package svl.learn.java.junit.dynamic;

import org.junit.ClassRule;
import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertTrue;

@RunWith(MyTestRunner.class)
public class MyTest {
    @ClassRule public static ParametrizeRule parametrizeRule = new ParametrizeRule();

    @Test public void basic() {
        System.out.println("111");
        assertTrue(true);
    }
}