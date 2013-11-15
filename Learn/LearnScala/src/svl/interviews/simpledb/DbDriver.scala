package svl.interviews.simpledb

object DbDriver {
    def main(args: Array[String]) {
        val db = new MemoryDatabase[String, String] with DatabaseTransaction[String, String]
        val CommandTemplate = """\s*([a-zA-Z0-9]*)\s*([a-zA-Z0-9]*)\s*([a-zA-Z0-9]*).*""".r

        var cont = true

        while(cont){
            val line = Console.readLine()
            line match {
                case CommandTemplate("set", key, value) => db.set(key, value)
                case CommandTemplate("get", key, "") => db.get(key)
                case CommandTemplate("count", value, "") => db.count(value.trim)
                case CommandTemplate("delete", key, "") => db.delete(key.trim)
                case CommandTemplate("begin", "", "") => db.beginTrans()
                case CommandTemplate("commit", "", "") => db.commit()
                case CommandTemplate("rollback", "", "") => db.rollback()
                case CommandTemplate("end", "", "") => cont = false
                case _ => println(s"Unknown command: $line")
            }
        }
    }
}
