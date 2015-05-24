package svl.learn.kotlin.classes

fun testProperties () {
    val person1 = PersonClass("Vic", "Spivak")
    println(person1.FirstName + " " + person1.LastName)
    person1.FirstName = "Lucy"
    println(person1.FirstName + " " + person1.LastName)
}
class PersonClass (firstName : String, lastName : String) {
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
