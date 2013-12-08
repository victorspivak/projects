import java.io.{PrintStream}

object FunctionsAsControlStruct {
    def main(args:Array[String]) {
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
    }
}



