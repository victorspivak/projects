package svl.lendingclub

import scala.collection.mutable

object Analyser {
    def main(args: Array[String]) {
        val filename = if (args.isEmpty) "data/lending-data1.csv"  else args(0)
        val latestDate = if (args.length < 2) "01/01/2015" else args(1)
        println(s"filename: $filename  latestDate: $latestDate")

        val parser = CsvParser(filename, latestDate)
        val header = parser.header
        val validData = parser.validData

        println("********** Parameters *******************")
        header.dump()
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

        val headersToProcess = List(
//            header.getIndex("loan_amnt"),
//            header.getIndex("revol_util"),
            header.getIndex("term"),
            header.getIndex("grade"),
            header.getIndex("sub_grade"),
            header.getIndex("home_ownership"),
            header.getIndex("emp_length"),
            header.getIndex("is_inc_v"),
            header.getIndex("purpose"),
            header.getIndex("addr_state"),
            header.getIndex("fico_range_high")
        )

        headersToProcess.foreach(index => {
            val paramStats = normalizeData(index)
            println("******************* %s *******************".format (header.label(index)))
            paramStats.foreach(println)
            println()
        })

        def normalizeData (index:Int) =
            validData.foldLeft(new mutable.HashMap[String, DataStats])((map, note) => {
                val value = note.get(index)
                map.get(value) match {
                    case Some(entry) => map.put(value, entry.updateStats(note))
                    case None => map.put(value, DataStats(value, note))
                }
                map
            }).values.toList.sortWith((first, second) => {first.probability > second.probability })
    }
}

