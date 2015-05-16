package svl.learn.kotlin.Containers

fun lists() {
    val l = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9)
    l.filter { it % 3 == 0 } . sortDescending() . forEach { print ("$it ") }
    println()
}