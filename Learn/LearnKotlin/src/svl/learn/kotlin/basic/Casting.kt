package svl.learn.kotlin.basic

fun testCasting() {
    processObj("Hello")
    processObj(50)
}

fun processObj(p:Any) {
    when(p) {
        is String -> println(p.toUpperCase())
        is Int -> println(p * 10)
    }
}