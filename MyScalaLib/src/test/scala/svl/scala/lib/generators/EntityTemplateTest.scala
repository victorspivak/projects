package svl.scala.lib.generators

import org.junit.Assert._
import org.junit.Test
import svl.scala.lib.generators.EntityTemplate.{RandomFixedString, RandomString, ConstString}

class EntityTemplateTest() {
  @Test def constString() {
    val constString = ConstString("foo")
    val attr:String = constString

    assertEquals("foo", attr)
  }

  @Test def randomString() {
    val randomString = RandomString("%s", 10)
    val val1:String = randomString
    val val2:String = randomString

    assertFalse(val1.equals(val2))
  }

  @Test def randomFixedLengthString() {
    val length: Int = 10
    val randomString = RandomFixedString("%s", length)
    val val1:String = randomString
    val val2:String = randomString

    assertEquals(length, val1.length)
    assertEquals(length, val2.length)
    assertFalse(val1.equals(val2))
  }
}
