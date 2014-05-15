package svl.learn.scala.classes

/*
 * User: victor    Date: 5/15/14   Time: 2:14 AM
 */
object Generics {
  def main(args: Array[String]) {
    trait Addable[A] {
      def add(a:A, b:A):A
      def zero:A
    }

    def reduce[A](list:List[A])(implicit A:Addable[A]): A =
      list.foldLeft(A.zero)(A.add)

    implicit val IntAsAddable =
      new Addable[Int] {
        override def add(a: Int, b: Int) = a + b
        override def zero = 0
      }

    implicit val StringAsAddable =
      new Addable[String] {
        override def add(a: String, b: String) = a + b
        override def zero = ""
      }

    println(reduce(List(1,2,3)))
    println("Empty list:")
    println(reduce(List.empty[Int]))
    println(reduce(List("hello", " ", "world", "!!!")))
  }
}
