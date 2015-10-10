package svl.learn.java.junit;

import junit.framework.TestSuite;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.AllTests;

import static org.junit.Assert.assertTrue;
import static svl.learn.java.junit.JUnitParametrizedHelper.*;

@RunWith(AllTests.class)
public class CustomJunitTest{
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
