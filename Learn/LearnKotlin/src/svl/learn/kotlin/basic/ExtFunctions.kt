package svl.learn.kotlin.basic

import java.util.List

fun testExtFunc() {
    fun List<Int>.swap(x : Int, y : Int) {
        val tmp = this[x] // 'this' corresponds to the list
        this[x] = this[y]
        this[y] = tmp
    }

    val l = arrayList(1, 2, 3)
    println(l)
    l.swap(0, 2)
    println(l)
}