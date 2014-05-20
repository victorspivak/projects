package svl.learn.scala.classes

import scala.language.implicitConversions

object ValueClasses {
  case class PersonName(imp:String) extends AnyVal
  case class PersonSSN(imp:String) extends AnyVal
  case class Year(imp:Int) extends AnyVal
  case class Person(name:PersonName, ssn:PersonSSN)

  implicit def Year2Int(year:Year) = year.imp
  implicit def Ssn2String(ssn:PersonSSN) = ssn.imp

  def main(args: Array[String]) {
    val victor = Person(PersonName("Victor"), PersonSSN("111-11-1111"))
    val yegor = Person(PersonName("Yegor"), PersonSSN("222-22-2222"))

    println(victor.ssn)

    val ssn = victor.ssn

    println(ssn.getClass)

    val year = Year(2014)
    val y:Int = year

    val strings = List[String](victor.ssn, yegor.ssn)
    strings.foreach(println)

    val ssns = List[PersonSSN](victor.ssn, yegor.ssn)
    ssns.foreach(println)
  }
}
