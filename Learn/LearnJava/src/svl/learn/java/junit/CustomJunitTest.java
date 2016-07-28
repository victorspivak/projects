package svl.learn.java.junit;

import junit.framework.TestSuite;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.AllTests;
import svl.learn.java.junit.rules.RuleHelloWorld;

import static org.junit.Assert.assertTrue;
import static svl.learn.java.junit.JUnitParametrizedHelper.*;

@RunWith(AllTests.class)
public class CustomJunitTest{
    @ClassRule static public RuleHelloWorld helloWorldClassLevel = new RuleHelloWorld();

    private int param;

    public CustomJunitTest(Integer param) {
        this.param = param;
    }

    public static synchronized TestSuite suite() throws Exception {
        return createSuite(2, 3);
    }

    @Before
    public void before() {
        param = 10 * param;
    }

    @Test
    public void oneTest() {
        assertTrue(param > 1);
    }

    @Test
    public void isBeforeCalled() {
        assertTrue("Before must be called", param > 10);
    }
}
