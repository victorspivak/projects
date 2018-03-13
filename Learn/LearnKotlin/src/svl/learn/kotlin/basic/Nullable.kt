package svl.learn.kotlin.basic

fun testNullable () {
    val v1:String? = null
    val len = v1?.length

    val lenAsString = v1?.let{"" + it.length} ?: "No String"

    println(lenAsString)

    try {
        println ("Length is $len.")
    }
    catch (e : Exception) {
        println(e)
    }
}