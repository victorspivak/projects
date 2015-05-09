package svl.learn.java.junit;

import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * CustomJunitTest
 *
 * @author victor.spivak
 * @since 196
 */
public class CustomJunit3Test extends TestCase{
    private int param;

    public CustomJunit3Test(String name, Integer param) {
        super(name);
        this.param = param;
    }

    @Override
    protected void runTest() throws Throwable {
        JUnit3ParametrizedHelper.runTest(this);
    }

    public static synchronized TestSuite suite() throws Exception {
        return JUnit3ParametrizedHelper.createSuite(CustomJunit3Test.class, 2, 3);
    }

    public void testOne() {
        assertTrue(param > 1);
    }

    public void testTwo() {
        assertTrue(param == 2);
    }
}
