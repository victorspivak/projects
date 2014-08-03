package svl.learn.scala.generics

object Covarians extends App{
  class Animal
  class Predator extends Animal
  class Canidae extends Predator
  class Dog extends Canidae
  class Wolf extends Canidae
  class Felidae extends Predator
  class Cat extends Felidae

  case class Cage1[+T<:Predator](private val _predator:T)

  val cage11 = Cage1(new Dog)
  println(cage11)

  //The following statement will not compile if we will not have +T in the Cage definition
  val cage12:Cage1[Predator] = cage11
  println(cage12)

  case class Cage2[T<:Predator](private var _predator:T) {
    def predator = _predator
    def predator_=[V <:T](value:V) {
      _predator = value
    }
  }

  val cage21 = Cage2(new Canidae)
  cage21.predator = new Dog()
  println(cage21)

  val dogs:List[Dog] = List(new Dog())
  val cats:List[Cat] = List(new Cat())
  val predators:List[Predator] = dogs ++ cats

  println(predators)
}
