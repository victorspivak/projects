package svl.learn.kotlin.containers

fun testSequences() {
    generateSequence (1, { x -> x + 1}) . filter {it % 6 == 0} . take(10) . forEach { print("$it ") }
    println()

    val s1 = generateSequence (1, { x -> x + 1}) . filter {it % 8 == 0} . take(5) . fold (0, { s, it -> s + it})
    println("s1 = $s1")
}