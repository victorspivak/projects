package svl.lendingclub

import java.text.SimpleDateFormat
import java.util.Date

object Statuses extends Enumeration {
    val Failed, Paid, Current, Late30, Late120, Skip = Value

    private val statusMapping = Map(
          "Does not meet the current credit policy  Status: Current" -> Current
        , "Does not meet the credit policy.  Status:Current" -> Current
        , "Late (31-120 days)" -> Failed
        , "Late (16-30 days)" -> Late30
        , "In Review" -> Skip
        , "In Funding" -> Skip
        , "Issuing" -> Skip
        , "Performing Payment Plan" -> Current
        , "Issued" -> Current
        , "Does not meet the current credit policy  Status: Fully Paid" -> Paid
        , "Does not meet the credit policy.  Status:Fully Paid" -> Paid
        , "Default" -> Failed
        , "Does not meet the current credit policy  Status: Performing Payment Plan" -> Skip
        , "Does not meet the credit policy.  Status:Performing Payment Plan" -> Skip
        , "Does not meet the current credit policy  Status: Late (16-30 days)" ->  Skip
        , "Does not meet the credit policy.  Status:Late (16-30 days)" ->  Skip
        , "Does not meet the current credit policy  Status: Late (31-120 days)" -> Skip
        , "Does not meet the credit policy.  Status:Late (31-120 days)" -> Skip
        , "Does not meet the current credit policy  Status: Charged Off" -> Skip
        , "Does not meet the credit policy.  Status:Charged Off" -> Skip
        , "In Grace Period" -> Skip
        , "Does not meet the current credit policy  Status: Default" -> Skip
        , "Does not meet the credit policy.  Status:Default" -> Skip
        , "Charged Off" -> Failed
        , "Current" -> Current
        , "Does not meet the current credit policy  Status: In Grace Period" -> Skip
        , "Does not meet the credit policy.  Status:In Grace Period" -> Skip
        , "Fully Paid" -> Paid
        , "" -> Skip
    )

//    private val statusMapping1 = Map("Does not meet the current credit policy  Status: Current" -> Current
//        , "Late (31-120 days)" -> Late120
//        , "Late (16-30 days)" -> Late30
//        , "In Review" -> Skip
//        , "Performing Payment Plan" -> Current
//        , "Issued" -> Current
//        , "Does not meet the current credit policy  Status: Fully Paid" -> Paid
//        , "Default" -> Failed
//        , "Does not meet the current credit policy  Status: Performing Payment Plan" -> Current
//        , "Does not meet the current credit policy  Status: Late (16-30 days)" ->  Late30
//        , "Does not meet the current credit policy  Status: Late (31-120 days)" -> Late120
//        , "Does not meet the current credit policy  Status: Charged Off" -> Failed
//        , "In Grace Period" -> Skip
//        , "Does not meet the current credit policy  Status: Default" -> Failed
//        , "Charged Off" -> Failed
//        , "Current" -> Current
//        , "Does not meet the current credit policy  Status: In Grace Period" -> Skip
//        , "Fully Paid" -> Paid
//        , "" -> Skip
//    )

    def toStatus(value:String) = statusMapping get value.trim() match {
        case Some(status) => status
        case None => throw new IllegalArgumentException("Unknown status: " + value);
    }
}

case class Headers(headers:Array[String]) {
    lazy val statusIndex     = getIndex("loan_status")
    lazy val issueDateIndex  = getIndex("issue_d")
    def getIndex(name:String) = {
        val index = headers.indexOf(name)
        if (index < 0) throw new IllegalArgumentException(s"Unknown header name: $name")
        index
    }
    def size = headers.length
    def dump() = headers.zipWithIndex.foreach{case (value, index) => println("%2d => %s".format(index, value))}
    def label(index:Int) = headers(index)

    normalizeHeaderNames()
    def normalizeHeaderNames(): Unit = {
        update("Status", "loan_status")
        update("Note Issue Date", "issue_d")
        update("Term", "term")
        update("PrincipalRemaining", "loan_amnt")
        update("InterestRate", "int_rate")
        update("Grade", "grade")

        def update(oldName:String, newName:String): Unit = {
            val index = headers.indexOf(oldName)
            if (index >= 0)
                headers(index) = newName
        }
    }

    override def toString = headers.mkString("|") + "\n"
}

case class Note(headers:Headers, values:Array[String]) {
    object DataParser{
        val monthYearFormat = new SimpleDateFormat("MMM-yyyy")
        val monthDayYearFormat = new SimpleDateFormat("mm/dd/yyyy")

        def parse(str:String) = {
            if (str.indexOf('/') >= 0)
                monthDayYearFormat.parse(str)
            else
                monthYearFormat.parse(str)
        }
    }

    def isValid = headers.size == values.length
    def status() = Statuses.toStatus(values(headers.statusIndex))
    def needProcessing(latestDate:Date) = {
        isValid && status() != Statuses.Skip && getIssueDate.compareTo(latestDate) < 0
    }
    def isFailed = isValid && status == Statuses.Failed
    def get(index:Int) = values(index).trim
    def getIssueDate = DataParser.parse(get(headers.issueDateIndex))

    override def toString = values.mkString(" | ") + "\n"
}

case class NoteAttribute(name:String, headerIndex:Int, ranges:List[Int]){
    def valueSlot(value:String) = {
        if (ranges.isEmpty)
            value
        else {
            val cleanValue = if (value.endsWith("%")) value.substring(0, value.length - 1) else value
            if (!cleanValue.isEmpty){
                val number = cleanValue.toDouble
                ranges.find(number <= _).getOrElse(9999999).toString
            } else
                ""
        }
    }
}

object NoteAttribute{
    def apply(headers:Headers, name:String, ranges:List[Int] = List()) = new NoteAttribute(name, headers.getIndex(name), ranges)
}

case class DataStats(value:String, var successCount:Int, var failCount:Int) {
    def updateStats(note:Note) = {
        if (note.isFailed)
            failCount += 1
        else
            successCount += 1
        this
    }

    def totalCount = successCount + failCount
    def probability = 100.0 * failCount / totalCount

    override def toString = "%20s ==> %6d / %6d = %6.2f".format(value.toString, failCount, totalCount, probability)
}

object DataStats {
    def apply(value:String):DataStats = DataStats(value, 0,0)
    def apply(value:String, note:Note):DataStats = DataStats(value, 0,0).updateStats(note)
}
