package svl.learn.java.junit;

import junit.framework.TestSuite;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.AllTests;

import static org.junit.Assert.assertTrue;
import static svl.learn.java.junit.JUnitParametrizedHelper.createSuite;

@RunWith(AllTests.class)
public class DerivedTest extends MyBaseTest {

    public static synchronized TestSuite suite() throws Exception {
        return createSuite();
    }

    @Test
    public void testBaseBeforeCalled() {
        assertTrue(getInit());
    }
}
