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
  }
}
