package svl.learn.java.junit;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.AllTests;
import svl.learn.java.junit.JUnitParametrizedHelper.TestDataEntry;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static svl.learn.java.junit.JUnitParametrizedHelper.TestDataProvider;
import static svl.learn.java.junit.JUnitParametrizedHelper.createSuite;

@RunWith(AllTests.class)
public class CombinedTest extends TestCase {
    private int param;

    public CombinedTest(Integer param) {
        this.param = param;
    }

    public static synchronized TestSuite suite() throws Exception {
        return createSuite(2, 3);
    }

    @Override
    protected void setUp() throws Exception {
        param = 10 * param;
    }

    public void testOne() {
        assertTrue(param > 10);
    }

    @Test
    public void one() {
    }

    public void testOne(int foo) {

    }

    @TestDataProvider(method = "multipliers")
    public void multiply(int x, int y, int result) {
        assertEquals(result, x * y);
    }

    public static TestDataEntry[] multipliers() {
        return new TestDataEntry[]{
            new TestDataEntry("basic", 2, 3, 6),
            new TestDataEntry("multiply by 0", 2, 0, 0),
        };
    }
}
