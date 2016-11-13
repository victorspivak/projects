package svl.learn.kotlin.classes

fun testProperties () {
    val person1 = PersonClass("Vic", "Spivak")
    println(person1.FirstName + " " + person1.LastName)
    person1.FirstName = "Lucy"
    println(person1.FirstName + " " + person1.LastName)
}

class PersonClass (firstName : String, lastName : String) {
    private var _firstName = firstName
    private var _lastName = lastName

    var FirstName : String
        get() {
            println ("Getter was invoked")
            return _firstName                  //It is access to the backing field
        }
        set (name) {
            println ("Setter was invoked")
            _firstName = name
        }

    val LastName : String = lastName
}
