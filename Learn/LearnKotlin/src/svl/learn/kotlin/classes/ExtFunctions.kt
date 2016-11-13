package svl.learn.kotlin.classes

import java.util.*

fun testExtFunc() {
    fun ArrayList<Int>.swap(x : Int, y : Int) {
        val tmp = this[x] // 'this' corresponds to the list
        this[x] = this[y]
        this[y] = tmp
    }

    val l = arrayListOf(1, 2, 3)
    println(l)
    l.swap(0, 2)
    println(l)


    fun Any?.hashCode(): Int {
        if (this == null) return 0
        // after the null check, 'this' is autocast to a non-null type, so the toString() below
        // resolves to the member function of the Any class
        return hashCode()
    }

    val s:String? = null
    println("HashCode for null: ${s.hashCode()}")
}