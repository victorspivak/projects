/*
 * User: Victor    Date: 1/21/12   Time: 12:16 AM
 */

object Strings {
    def main(args:Array[String]) {
        println("""This string could have " and ' and new line
            Type "HELP" for help.""")

        println ("To handle left whitespaces we should use pipe symbol and call stripMargin:")

        println("""|This string could have " and ' and new line
                  |Type "HELP" for help.""".stripMargin)

        Console println "Hello World".indexOf("l")
    }
}

