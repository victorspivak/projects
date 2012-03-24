import io.Source

/*
* User: Victor    Date: 1/20/12   Time: 11:26 PM
*/

val filename = "src/svl/learn/scala/files/ReadFile.scala"
for (line <- Source.fromFile(filename).getLines())
    println(line.length +" "+ line)

val allLines = Source.fromFile(filename).getLines().toList
Console println  allLines
