package svl.learn.kotlin.basic

import kotlin.reflect.KClass
import kotlin.reflect.full.declaredMemberProperties
import kotlin.reflect.full.memberExtensionProperties
import kotlin.reflect.full.memberProperties

class Foo(val name:String, val id:Int) {
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

    println("Properties: ${clazz.memberProperties}")
    println("Extension Properties: ${clazz.memberExtensionProperties}")
    Foo("Name", 111).objCouner

    fun isOdd(x: Int) = x % 2 != 0
    val numbers = listOf(1, 2, 3)
    println(numbers.filter(::isOdd))
}