package svl

import org.junit._
import Assert._

class Tests {

  @Test def sample1() {
    assertEquals(6, Hello.sum(2,4))
  }

  @Test def sample2() {
    assertEquals(8, Hello.sum(4,4))
  }
}
