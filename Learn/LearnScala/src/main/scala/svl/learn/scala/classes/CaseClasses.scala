package svl.learn.scala.classes

object CaseClasses {
  def main(args: Array[String]) {
    case class Person(firstName:String, lastName:String, age:Int){
      def updateAge(newAge:Int) = this.copy(age = newAge)
    }

    val person5years = Person("John", "Smith", 5)
    val person10years = person5years.updateAge(10)

    println(person5years)
    println(person10years)

    //case class from tuple
    val p1 = Person.tupled("Vic", "Smith", 22)
    println(p1)

    //tuple from a case class
    val t1 = Person.unapply(p1).get
    println(t1)

    println("===============================================================")
    println("Arity: " + p1.productArity)
    for (i <- 0 until p1.productArity)
      println(s"$i -> ${p1.productElement(i)}")

    println("===============================================================")
    case class User(private var _login:String, var password:String){
      def login = _login

      def login_=(value:String){
        println(s"Reset login from $login to $value")
        _login = value
      }
    }

    val user1 = User("v", "p")
    println(user1)
    user1.login = "Victor"
    println(user1)
  }
}
