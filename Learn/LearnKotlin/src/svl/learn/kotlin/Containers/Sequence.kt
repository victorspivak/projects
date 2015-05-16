package svl.learn.kotlin.Containers

fun sequences() {
    sequence (1, { x -> x + 1}) . filter {it % 6 == 0} . take(10) . forEach { print("$it ") }
    println()

    val s1 = sequence (1, { x -> x + 1}) . filter {it % 8 == 0} . take(5) . fold (0, {(s, it) -> s + it})
    println("s1 = $s1")
}