package svl.learn.kotlin.classes

enum class ProtocolState {
    WAITING {
        override fun signal() = TALKING
    }

    TALKING {
        override fun signal() = WAITING
    }

    abstract fun signal(): ProtocolState
}

fun testEnums() {
    val e = ProtocolState.WAITING
    println("Protocol switched to ${e.signal()}")
}