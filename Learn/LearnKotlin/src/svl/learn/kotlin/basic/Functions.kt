package svl.learn.kotlin.basic

fun testFunctions() {
    fun join<T>(data:Collection<T>, prefix : String = "", delimiter :String = " ", suffix:String = "") : String {
        val builder = StringBuilder()
        builder.append(prefix)
        for (el in data)
            builder.append(el, delimiter)
        builder.append(suffix)
        return builder.toString()
    }

    val d = listOf('H', 'e', 'l', 'l', 'o')
    println(d.joinToString())
    println(join(d))
    println(join(d, delimiter = ""))
}