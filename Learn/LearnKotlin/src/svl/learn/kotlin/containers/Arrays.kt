package svl.learn.kotlin.containers

fun testArrays() {
    val asc = Array(5, {i -> (i * i).toString()})
    println(asc.joinToString())
}