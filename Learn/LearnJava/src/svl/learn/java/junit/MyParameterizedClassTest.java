package svl.learn.java.junit;

/**
 * MyParameterizedClassTest
 *
 * @author victor.spivak
 * @since 196
 */
import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import static org.junit.Assert.assertEquals;

@RunWith(Parameterized.class)
public class MyParameterizedClassTest {

    public class MyClass {
      public int multiply(int x, int y) {
        return x * y;
      }
    }

  private int multiplier1;
  private int multiplier2;
  private int result;

  public MyParameterizedClassTest(int testParameter1, int testParameter2, int result) {
    this.multiplier1 = testParameter1;
    this.multiplier2 = testParameter2;
    this.result = result;
  }

  // creates the test data
  @Parameters
  public static Collection<Object[]> data() {
    Object[][] data = new Object[][] { { 1, 1, 1 }, { 5, 10, 50 }, { 11, 11, 121 } };
    return Arrays.asList(data);
  }

  @Test
  public void testMultiplyException() {
    MyClass tester = new MyClass();
    assertEquals("Result", result, tester.multiply(multiplier1, multiplier2));
  }

}
