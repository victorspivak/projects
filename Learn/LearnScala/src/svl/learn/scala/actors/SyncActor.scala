import actors.Actor._

object AyncActor {
    def main(args:Array[String]) {
        val calc = actor {
            loop {
                react {
                    case (a:Int, b:Int, "+") =>
                        sender ! (a + b)
                    case (a:Int, b:Int, "-") =>
                        sender ! (a - b)
                    case (a:Int, b:Int, "*") =>
                        Thread.sleep(2000)
                        sender ! (a * b)
                    case "EXIT" =>
                        exit()
                    case x:Any =>
                        println("Unknown command")
                        sender ! "Unknown operations: " + x
                }
            }
        }

        calc.start()

        var res = calc !? (1, 2, "+")
        println(res)
        res = calc !? (1, 2, "-")
        println(res)

        calc ! (10, 20, "+")
        res = self.?
        println(res)

        val f = calc !! (2, 3, "*")
        println(f())

        calc ! "EXIT"
    }
}

