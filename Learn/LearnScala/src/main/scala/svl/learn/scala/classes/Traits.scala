package svl.learn.scala.classes

object Traits {
  def main(args: Array[String]) {
    trait Db[K] {
      def insert(k: K)

      def delete(k: K)
    }

    trait Transaction[K] extends Db[K] {
      this: Db[K] =>

      abstract override def insert(k: K) {
        println("From Tr")
        super.insert(k)
      }

      abstract override def delete(k: K) {
        println("From Tr")
        super.delete(k)
      }
    }

    class TestDb[K] extends Db[K] {
      def insert(k: K) {
        println(s"Insert $k")
      }

      def delete(k: K) {
        println(s"Delete $k")
      }
    }

    val db = new TestDb[String] with Transaction[String]
    db.insert("1")

    trait A {
      val fooAsVal:String
      def fooAsDef:String
    }

    trait B extends A {
      val hello1 = fooAsVal + " World"
      lazy val hello111 = fooAsVal + " World"
      val hello2 = fooAsDef + " World"
      def hello11 = fooAsVal + " World"
      def hello12 = fooAsDef + " World"
    }

    class C extends B {
      val fooAsVal = "Hello"
      def fooAsDef = "Hello"
    }

    val c = new C

    println(c.hello1)
    println(c.hello111)
    println(c.hello2)
    println(c.hello11)
    println(c.hello12)
  }
}
