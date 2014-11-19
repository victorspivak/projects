package svl.lendingclub

import java.text.SimpleDateFormat
import java.util.Date

import io.Source
import collection.mutable.ArrayBuffer

class CsvParser (filename:String, latestDate:Date) {
    private val source = Source.fromFile(filename, "utf-8").getLines()
    source.drop(1)
    val header = Headers(source.next().split(",").map(_.trim).map(removeEnclosedQuotes))
    
    private val fixedSource = source.filter(!_.isEmpty).map(appendSpaceIfNeeded).map(processCommasInQuotas)
    private val allData = fixedSource.foldLeft(new ArrayBuffer[Note])((buf, el) => buf += stringToNote(el))

    val validData = allData.filter(_.needProcessing(latestDate))

    private def stringToNote(str:String) = Note(header, str.split(",").map(_.trim).map(removeEnclosedQuotes))

    private def removeEnclosedQuotes (line:String): String = {
        if (!line.isEmpty && line(0) == line.last && line(0) == '\"')
            line.substring(1, line.length - 1)
        else
            line
    }

    private def appendSpaceIfNeeded (line:String): String = {
        line.last match {
            case ',' => line + " "
            case _ => line
        }
    }

    private def processCommasInQuotas (line:String):String  = {
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
}

object CsvParser {
    def apply (filename:String, latestDate:String) = new CsvParser(filename, new SimpleDateFormat("MM/dd/yyyy").parse(latestDate))
}
