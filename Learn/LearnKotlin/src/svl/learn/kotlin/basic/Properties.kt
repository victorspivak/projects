package svl.learn.kotlin.basic

fun testProperties () {
    val person1 = Person("Vic", "Spivak")
    println(person1.FirstName + " " + person1.LastName)
    person1.FirstName = "Lucy"
    println(person1.FirstName + " " + person1.LastName)
}
class Person (firstName : String, lastName : String) {
    public var FirstName : String = firstName
        get() {
            println ("Getter was invoked")
            return $FirstName                  //It is access to the backing field
        }
        set (name) {
            println ("Setter was invoked")
            $FirstName = name;
        }

    public val LastName : String = lastName

}
