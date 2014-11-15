package svl.lendingclub

object Test extends App {
    val str1 = """Hello, world "It is in, the quote" and, and "there is no comma" "It is in, the quote" and it is, out """

    def processCommasInQuotas1 (line:String):String  = {
        val pattern = """([^\"]*)(\")([^\"\,]*)([,])([^\"]*)(\")(.*)""".r

        line match {
            case pattern(p1, p2, p3, p4, p5, p6, p7) => processCommasInQuotas1(p1 + "''" + p3 + "'" + p5 + "''" + p7)
            case _ => line
        }
    }

    def processCommasInQuotas (line:String):String  = {
        var inQuotes = false
        val replaced = line.map {
            case ch@'\"' => inQuotes = !inQuotes; ch
            case ',' if inQuotes => '\''
            case ch => ch
        }
        
        if (inQuotes)
            line
        else
            replaced.mkString
    }

    println(processCommasInQuotas1(str1))
    println(processCommasInQuotas(str1))
}

