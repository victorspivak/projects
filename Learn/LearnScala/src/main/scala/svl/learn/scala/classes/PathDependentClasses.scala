package svl.learn.scala.classes

/*
 * User: victor    Date: 10/2/14   Time: 12:50 PM
 */
object PathDependentClasses {
  def main(args: Array[String]) {
    class A {
      class B
      var b: Option[B] = None
    }
    val a1 = new A
    val a2 = new A
    val b1 = new a1.B
    val b2 = new a2.B
    a1.b = Some(b1)

    // The following line does not compile
    // a2.b = Some(b1)
  }
}
