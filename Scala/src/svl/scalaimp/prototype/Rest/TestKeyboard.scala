package svl.scalaimp.prototype.Rest

import tools.jline.console.history.MemoryHistory

/*
 * User: victor    Date: 2/4/13   Time: 2:33 AM
 */
object TestKeyboard {
    def main(args: Array[String]) {
        val con = new tools.jline.console.ConsoleReader()
        con.setHistory(new MemoryHistory)
        while(true) {
            con.setHistoryEnabled(true)
//            println( con.readVirtualKey() )




            println(con.readLine())
        }
    }
}
