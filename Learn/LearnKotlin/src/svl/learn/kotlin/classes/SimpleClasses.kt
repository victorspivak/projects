package svl.learn.kotlin.classes

data class Person(val firstName:String, val lastName:String, val age:Int)

class Calculator {
    var value:Int = 0
    infix fun set(v:Int) {
        value = v
    }

    infix fun add(v:Int) {
        value += v
    }

}
fun testSimpleClasses() {
    val p1 = Person("Vic", "Spivak", 17)
    val p2 = p1.copy(age = 18)
    println(p1)
    println(p2)

    val(f, l, age) = p1
    println("$f $l is $age")

    //Using infix form calling functions with one parameter:
    val calc = Calculator()
    calc set 10
    calc add 40
    println("Calculator: ${calc.value}")
}

