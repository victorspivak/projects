package svl.learn.kotlin.classes

class ClassInitializers {
    init {
        println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Init should be called for each object")
    }
    companion object {
        init {
            println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>InitClass should be called once")
        }
    }
}
fun testClassInitializers() {
    val o1 = ClassInitializers()
    val o2 = ClassInitializers()
}
