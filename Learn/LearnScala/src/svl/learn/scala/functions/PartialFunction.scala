package svl.learn.scala.functions

/*
 * User: victor    Date: 11/5/13   Time: 12:04 AM
 */
object PartialFunction {
    def main(args: Array[String]) {
        val f:String => String = {
            case "ping" => "pong"
            case "pong" => "ping"
            case _ => "????"
        }                                               //> f  : String => String = <function1>

        println(f("ping"))                                       //> res0: String = pong
        println(f("pong"))                                       //> res1: String = ping
        println(f("piig"))                                       //> res2: String = ????

        val f1:Function1[String, String] = {
            case "ping" => "pong"
            case "pong" => "ping"
            case _ => "????"
        }
        println(f1("ping"))                                       //> res0: String = pong
        println(f1("pong"))                                       //> res1: String = ping
        println(f1("piig"))                                       //> res2: String = ????

        val p:PartialFunction[String, String] = {
            case "ping" => "pong"
            case "pong" => "ping"
        }                                               //> p  : PartialFunction[String,String] = <function1>

        println(p.isDefinedAt("ping"))                           //> res3: Boolean = true
        println(p.isDefinedAt("pong"))                           //> res4: Boolean = true
        println(p.isDefinedAt("piig"))                           //> res5: Boolean = false


    }
}
