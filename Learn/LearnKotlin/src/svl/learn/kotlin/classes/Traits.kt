package svl.learn.kotlin.classes

trait Greeter {
    val expression:String
    fun greet(name:String) : String {
        return "$expression $name"
    }
}

class FormalGreeter : Greeter {
    override val expression: String = "Hello"
}

fun testTraits() {
    val fg = FormalGreeter()

    println(fg.greet("Victor"))
}