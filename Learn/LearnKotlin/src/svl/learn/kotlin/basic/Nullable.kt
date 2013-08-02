package svl.learn.kotlin.basic

fun testNullable () {
    var v1:String? = null;
    var len = v1?.length()

    try {
        println ("Length is ${len}. Class is ${len.javaClass}")
    }
    catch (e : Exception) {
        println(e)
    }
}