package svl.learn.kotlin.classes

class Outer1 {
    private val bar: Int = 1
    inner class Inner {
        fun foo() = bar
    }
}

class Outer2 {
    private val bar: Int = 1
    class Nested {
        //does not have access to bar
        fun foo() = 2
    }
}

fun testNestedClasses() {
    println("Inner class call: ${Outer1().Inner().foo()}")
    println("Nested class call: ${Outer2.Nested().foo()}")
}