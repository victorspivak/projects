import java.io.PrintStream

def withTiming(out: PrintStream)(op :  => Unit) {
    val start = System.currentTimeMillis()
    op
    val time = System.currentTimeMillis() - start
    out.println ("============> Tracing Time: " + time)
}


