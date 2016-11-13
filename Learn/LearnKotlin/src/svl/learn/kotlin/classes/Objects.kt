package svl.learn.kotlin.classes

object Singleton{
    fun foo() {
        println("I am ${this} foo method")
    }
}

class MyClass{
    object Helper {
        fun help() {
            println("It is MyClass Helper")
        }
    }

    companion object {
        fun help() {
            println("It is MyClass MyClassHelper")
        }
    }
}

open class InstanceCounter(){
    var count:Int = 0

    fun increment() = ++count
}

class MyCountedClass1{
    companion object : InstanceCounter()
    init {
        increment()
    }
}

class MyCountedClass2{
    companion object : InstanceCounter()
    init {
        increment()
    }
}

fun testObjects() {
    Singleton.foo()
    MyClass.help()
    MyClass.Helper.help()

    for (i in 1..5)
        MyCountedClass1()

    for (i in 1..10)
        MyCountedClass2()

    println("Counts: ${MyCountedClass1.count} ${MyCountedClass2.count}")
}