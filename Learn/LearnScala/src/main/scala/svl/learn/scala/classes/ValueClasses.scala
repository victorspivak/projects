package svl.learn.scala.classes

object ValueClasses {
  class PersonName(val imp:String) extends AnyVal
  class PersonSSN(val imp:String) extends AnyVal
  case class Person(name:PersonName, ssn:PersonSSN)

  def main(args: Array[String]) {
    val vic = Person(new PersonName("Victor"), new PersonSSN("111-11-1111"))

    println(vic.ssn)

    //val strings = List[String](vic.ssn)
    //ssns.foreach{ssn => println(ssn)}

    //val ssns = List[PersonSSN](vic.ssn)
    //ssns.foreach{ssn => println(ssn)}
  }
}
