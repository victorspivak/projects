import java.io.{PrintStream}

/*
* User: Victor    Date: 1/25/12   Time: 11:26 PM
*/

def withTracing(out: PrintStream)(op :  => Unit) {
    val start = System.currentTimeMillis()
    op
    val time = System.currentTimeMillis() - start
    out.println ("Tracing Time: " + time)
}

//Scala supports using {} instead of () when a calling function has only one parameter
withTracing(System.out) {
    val list = (1 to 100).toList
    list.foldLeft(0) (_+_)
}

withTracing(System.out) {
    val list = (1 to 1000).toList
    list.foldLeft(0) (_+_)
}

withTracing(System.out) {
    val list = (1 to 1000000).toList
    list.foldLeft(0) (_+_)
}


