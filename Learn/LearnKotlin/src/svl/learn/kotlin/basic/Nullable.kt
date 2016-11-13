package svl.learn.kotlin.basic

fun testNullable () {
    val v1:String? = null
    val len = v1?.length

    try {
        println ("Length is $len.")
    }
    catch (e : Exception) {
        println(e)
    }
}