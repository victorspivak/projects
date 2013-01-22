import io.Source
import java.io.{FileNotFoundException, IOException}
import scala.util.control.Exception.catching

object Exceptions {
    def main(args:Array[String]) {
        try {
            val i = 5/0
        } catch {
            case e:ArithmeticException => Console println "Expected exception even when asserts are disabled"
        }


        //Use of Catch Class
        val fileName = "c:\\temp\\test.txt"

        val fileCatch = catching(classOf[FileNotFoundException], classOf[IOException]).withApply(e => throw new RuntimeException(e))
        val content = fileCatch.either{Source.fromFile(fileName ).getLines.mkString("\n")}
        println(content.right.getOrElse("The file cannot be found or read"))

        def exceptionHandler[T] (body: => T):T = {
            try {
                body
            }
            catch {
                case e:NullPointerException => println("Null");  null.asInstanceOf[T]
                case e:IOException => println("IO Exception");  null.asInstanceOf[T]
                case e:Exception => println(e); null.asInstanceOf[T]
            }
        }

        exceptionHandler[String]({
            val fileName = "c:\\temp\\test.txt"
            Source.fromFile(fileName ).getLines.mkString("\n")
        })

        val res = exceptionHandler({
            2 + 2
        })

        println(res)
    }
}

