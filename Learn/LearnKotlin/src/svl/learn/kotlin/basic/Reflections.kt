package svl.learn.kotlin.basic

import kotlin.reflect.KClass

class Foo(public val name:String, public val id:Int) {
    fun foo() = name
}

var Foo.objCouner: Int
    get() {
    println ("objCouner Getter was invoked ${this.javaClass}")
    return 0
}
    set (name) {
        println ("objCouner Setter was invoked")
    }


fun testReflections(){
    println("Reflections: >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    val clazz = Foo::class

    println("Properties: ${clazz.properties}")
    println("Extension Properties: ${clazz.extensionProperties}")
    Foo("Name", 111).objCouner

    fun isOdd(x: Int) = x % 2 != 0
    val numbers = listOf(1, 2, 3)
    println(numbers.filter(::isOdd))
}