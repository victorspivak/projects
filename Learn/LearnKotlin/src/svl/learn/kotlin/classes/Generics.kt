package svl.learn.kotlin.classes

data class MyPerson(var name:String, var age:Int)

//reified could be used only with inline functions. It solves the type erasure problem
inline fun <reified T> membersOf() = T::class.members

inline fun <reified T> factory(vararg args:Any) :T where T: Any {
    val types = args.map(::typeExtrator).toTypedArray()

    return T::class.java.getConstructor(*types).newInstance(*args)
}

fun typeExtrator(v:Any):Class<out Any> {
    val res = when (v) {
        is Int -> Int::class.java
        else -> v.javaClass
    }
    return res
}

fun testGenerics() {
    println(membersOf<MyPerson>().joinToString("\n\t"))
    println(factory<String>("aaa"))
    println(factory<MyPerson>("Victor Spivak", 18))
}
