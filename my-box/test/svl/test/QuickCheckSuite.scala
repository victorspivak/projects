package svl.test

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class QuickCheckSuite extends FunSuite with Checkers {
  test("String Utils satisfies properties.") {
    check(new QuickCheckStringUtils)
  }
}
