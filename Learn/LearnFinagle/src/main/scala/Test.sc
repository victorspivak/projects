case class Person(firstName:String, lastName:String, email:String)

val FirstName = "vic"

def foo(p:Person) = {
  p match {
    case Person(FirstName, ln, em) => ln
    case _ => "Oops"
  }
}

foo(Person("vic", "Spi", ""))

