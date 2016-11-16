package svl.learn.kotlin.basic

fun testFunctions() {
    fun<T> join(data:Collection<T>, prefix : String = "", delimiter :String = " ", suffix:String = "") : String {
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

    tailrec fun findFixPoint(x: Double = 1.0): Double
            = if (x == Math.cos(x)) x else findFixPoint(Math.cos(x))

    print(findFixPoint())

    varargExample()
}

private fun varargExample() {
    fun foo(vararg args: Int) {
        println(args.joinToString())
    }

    val numbers = intArrayOf(1, 2, 3)
    foo(1, 2, 3)
    foo(*numbers)
}