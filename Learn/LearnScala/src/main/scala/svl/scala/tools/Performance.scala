package svl.scala.tools

import java.io.PrintStream

object Performance {
    def withTiming(op :  => Unit) {
        withTiming(System.out) (op)
    }

    def withTiming(out: PrintStream)(op :  => Unit) {
        val start = System.currentTimeMillis()
        op
        val time = System.currentTimeMillis() - start
        out.println ("============> Tracing Time: " + time)
    }
}
