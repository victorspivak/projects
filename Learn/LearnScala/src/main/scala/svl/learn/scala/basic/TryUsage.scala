package svl.learn.scala.basic

import scala.util.{Try, Success, Failure}

/*
 * User: victor    Date: 10/12/13   Time: 3:46 PM
 */
object TryUsage {
    def main(args: Array[String]) {
        def divide: Try[Int] = {
            val dividend = Try(Console.readLine("Enter an Int that you'd like to divide:\n").toInt)
            val divisor = Try(Console.readLine("Enter an Int that you'd like to divide by:\n").toInt)
            val problem = dividend.flatMap(x => divisor.map(y => x/y))
            problem match {
                case Success(v) =>
                    println("Result of " + dividend.get + "/"+ divisor.get +" is: " + v)
                    Success(v)
                case Failure(e) =>
                    println("You must've divided by zero or entered something that's not an Int. Try again!")
                    println("Info from the exception: " + e.getMessage)
                    divide
            }
        }

        divide
    }
}
