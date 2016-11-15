package svl.learn.kotlin.classes

fun main(args: Array<String>) {
    testGenerics()
}

//reified could be used only with inline functions. It solves the type erasure problem
inline fun <reified T> membersOf() = T::class.members

inline fun <reified T> factory()  where T: Any = T::class.java.newInstance()

fun testGenerics() {
    data class Person(var name:String = "no name", var age:Int = 0)

    println(membersOf<Person>().joinToString("\n"))

    println("|" + factory<String>() + "|")
    //println("|" + factory<Person>() + "|")

//    val o = Person::class.java.getConstructor(String::class.java, Int::class.java).newInstance("aaa", 11)
    val o = Person()
    println("|" + o + "|")

    fun foo(vararg args:Int) {
        println(args.joinToString())
    }

    val numbers = intArrayOf(1, 2, 3)
    foo(1,2,3)
    foo(*numbers)
}