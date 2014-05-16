package svl.scalaz.validation

import scalaz._
import Scalaz._

object ValidationExample {
  def main(args: Array[String]) {
    type ErrorsOr[A] = ValidationNel[String, A]
    type Validator[A] = String => ErrorsOr[A]

    case class Foo(a: Int, b: Char, c: String)

    val checkA: Validator[Int] = (s: String) =>
      try s.toInt.success catch {
        case _: NumberFormatException => "Not a number!".failureNel
      }

    val checkB: Validator[Char] = (s: String) =>
      if (s.size != 1 || s.head < 'a' || s.head > 'z') {
        "Not a lower case letter!".failureNel
      } else s.head.success

    val checkC: Validator[String] = (s: String) =>
      if (s.size == 4) s.success else "Wrong size!".failureNel

    def validateFoo(a: String, b: String, c: String) =
      (checkA(a) |@| checkB(b) |@| checkC(c))(Foo.apply)

    println(validateFoo("15", "c", "abcd"))
    println(validateFoo("ab", "c", "ef"))
    println(validateFoo("ab", "cd", "ef"))
  }
}
