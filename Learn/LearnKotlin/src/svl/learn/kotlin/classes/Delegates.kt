package svl.learn.kotlin.classes

import kotlin.properties.Delegates.observable
import kotlin.reflect.KProperty

interface Base {
    fun print()
    fun process()
}

fun testDelegates() {
    class BaseImpl(var x: Int): Base {
        override fun process() {x *= 2}
        override fun print() { println(x) }
    }

    class Derived(val b: Base) : Base by b {
        override fun process() {
            b.process()
            b.process()
        }
    }

    val b = BaseImpl(10)
    val d = Derived(b)
    d.process()
    d.print()

    class MyPropertyDelegate<T>(var value:T) {
        operator fun getValue(thisRef: Any?, prop: KProperty<*>): T {
            println("$thisRef, thank you for delegating '${prop.name}' to me!")
            return value
        }

        operator fun setValue(thisRef: Any?, prop: KProperty<*>, v: T) {
            println("$v has been assigned to '${prop.name} in $thisRef.'")
            value = v
        }
    }

    class PropertyDelegateExample(v1:String, v2:String, v3:Int) {
        var p1: String by MyPropertyDelegate<String>(v1)
        var p2: String by MyPropertyDelegate<String>(v2)
        var p3: Int by MyPropertyDelegate<Int>(v3)
    }

    val o = PropertyDelegateExample("1", "2", 3)
    o.p1 = "10"
    o.p2 = "20"
    println("===> ${o.p1} ===> ${o.p2} ===> ${o.p3}")

    fun testStandardDelegates() {
        class TestStandardDelegates {
            val lazyValue: String by lazy {
                println("computed!")
                "Hello"
            }

            var name: String by observable("<no name>") {
                prop, old, new ->
                println("$old -> $new")
            }
        }

        val obj1 = TestStandardDelegates()
        println(obj1.lazyValue)
        println(obj1.lazyValue)
        val obj2 = TestStandardDelegates()
        println(obj2.lazyValue)
        println(obj2.lazyValue)
        obj1.name = "name 1"
        obj1.name = "name 2"
        obj1.name = "name 3"

        class User(val map: Map<String, Any?>) {
            val name: String by map
            val age: Int     by map
        }

        val user = User(mapOf(
                "name" to "John Doe",
                "age"  to 25
        ))

        println(user.name) // Prints "John Doe"
        println(user.age)  // Prints 25
    }

    testStandardDelegates()
}