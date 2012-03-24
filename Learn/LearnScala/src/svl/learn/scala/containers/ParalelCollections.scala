import java.io.PrintStream

def withTracing(out: PrintStream)(op :  => Unit) {
    val start = System.currentTimeMillis()
    op
    val time = System.currentTimeMillis() - start
    out.println ("Tracing Time: " + time)
}

withTracing(System.out){
    val list = (1 to 2000000).par.filter(_ % 37 != 0).sum
}

