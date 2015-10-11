package svl.learn.kotlin.classes

fun testDelegates() {
    trait Base {
        fun print()
        fun process()
    }

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
        fun get(thisRef: Any?, prop: PropertyMetadata): T {
            println("$thisRef, thank you for delegating '${prop.name}' to me!")
            return value
        }

        fun set(thisRef: Any?, prop: PropertyMetadata, v: T) {
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

}