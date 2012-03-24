/*
 * User: Victor    Date: 1/26/12   Time: 1:39 AM
 */

class Person (val first:String, val last:String) {
    override def toString = last + "," + first
}

object Person {
    def apply(first:String, last:String) = new Person(first, last)
}
val person1 = new Person("Victor", "Spivak")
Console println  person1

val person2 = Person("Victor", "Spivak")
Console println  person2

