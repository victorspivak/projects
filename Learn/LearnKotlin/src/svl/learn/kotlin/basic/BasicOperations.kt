package svl.learn.kotlin.basic

fun testWhen() {
    testWhen(1)
    testWhen(2)
    testWhen("Vic")
    testWhen(5)
    testWhen("Victor")
}

fun testWhen(p:Any?) {
    when(p) {
        1 -> println("It is one")
        2 -> println("It is two")
        "Vic" -> println("Hello $p")
        !is String -> println("It is not a String")
        else -> println("It is unknown")
    }
}

fun testLoops() {
    var s = 0
    for (i in 1..10)
        s += i

    println("Sum is $s")

    val l = listOf(1, 3, 5, 7, 9, 7, 5)
    for (e in l) print("$e ")
    println()

    //or
    for (i in l.indices) print("${l[i]} ")
    println()

    s = 0
    @myloop for (i in 1..10) {
        for (j in 1..10) {
            if (i == 2 && j > 5)
                break@myloop
            s += i
        }
    }

    println("Breaking top loop with $s as result")
}