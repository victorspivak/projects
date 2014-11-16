package svl.lendingclub

import scala.collection.mutable

object Analyser {
    def main(args: Array[String]) {
        val filename = if (args.isEmpty) "data/lending-data1.csv"  else args(0)
        val latestDate = if (args.length < 2) "01/01/2015" else args(1)
        println(s"filename: $filename  latestDate: $latestDate")

        val parser = CsvParser(filename, latestDate)
        val headers = parser.header
        val validData = parser.validData

        println("********** Parameters *******************")
        headers.dump()
        println()

        val statusStats = validData.foldLeft(new mutable.HashMap[Statuses.Value, Int])((map, entry) => {
            map.get(entry.status()) match {
                case Some(count) => map.put(entry.status(), count + 1)
                case None => map.put(entry.status(), 1)
            }
            map
        })
        val totalNotesCount = validData.size
        val failedNotesCount = validData.count(_.status == Statuses.Failed)
        val totalStats = DataStats("Total Stats", totalNotesCount - failedNotesCount, failedNotesCount)

        println("********** Statuses *******************")
        statusStats.foreach{case (key, value) => println ("%10s --> %6d".format(key, value))}
        println(totalStats)
        println()

        val attributesToProcess = List(
            NoteAttribute(headers, "term")
            ,NoteAttribute(headers, "loan_amnt", List(10000, 15000, 20000, 25000, 30000, 35000, 40000))
            ,NoteAttribute(headers, "revol_util", List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200))
            ,NoteAttribute(headers, "int_rate", List(8, 9, 10, 12, 14, 16, 18, 20, 25, 30, 100))
            ,NoteAttribute(headers, "annual_inc", List(25000, 50000, 60000, 70000, 80000, 90000, 10000, 150000, 200000))
            ,NoteAttribute(headers, "grade")
//            ,NoteAttribute(headers, "sub_grade")
            ,NoteAttribute(headers, "home_ownership")
            ,NoteAttribute(headers, "emp_length")
            ,NoteAttribute(headers, "is_inc_v")
            ,NoteAttribute(headers, "purpose")
//            ,NoteAttribute(headers, "addr_state")
            ,NoteAttribute(headers, "fico_range_high")
        )


        attributesToProcess.foreach(attribute => {
            val paramStats = normalizeData(attribute)
            println("********** %s ********** %s **********".format (headers.label(attribute.headerIndex), totalStats))
            paramStats.foreach(println)
            println()
        })

        def normalizeData (noteAttribute:NoteAttribute) =
            validData.foldLeft(new mutable.HashMap[String, DataStats])((map, note) => {
                val value = note.get(noteAttribute.headerIndex)
                val slot = noteAttribute.valueSlot(value)
                map.get(slot) match {
                    case Some(entry) => map.put(slot, entry.updateStats(note))
                    case None => map.put(slot, DataStats(slot, note))
                }
                map
            }).values.toList.sortWith((first, second) => {first.probability > second.probability })
    }
}

