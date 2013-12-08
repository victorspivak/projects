import io.Source

object ReadFile {
    def main(args:Array[String]) {
        val filename = "src/main.scala.svl/learn/scala/files/ReadFile.scala"
        for (line <- Source.fromFile(filename).getLines())
            println(line.length +" "+ line)

        val allLines = Source.fromFile(filename).getLines().toList
        Console println  allLines
    }
}

