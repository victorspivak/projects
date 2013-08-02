import io.Source

object NestedFunctions {
    def main(args:Array[String]) {
        def processLines1(lines: List[String], width: Int) {
            def processLine(width: Int, line: String) {
                if (line.length > width)
                    println("Exceed: " + line)
            }

            for (line <- lines) {
                processLine(width, line)
            }
        }

        val lines = List("Hello", "World", "It is veryyyyyyyyyyyyyyyy long lineeeeeeeeeeeeeeeeeeeeeeeee")
        processLines1(lines, 10)

        def processLines2(lines: List[String], width: Int) {
            def processLine(line: String) {
                //the width is in the scope and we do not need to pass it
                if (line.length > width)
                    println("Exceed: " + line)
            }

            for (line <- lines) {
                processLine(line)
            }
        }
        processLines2(lines, 10)
    }
}


