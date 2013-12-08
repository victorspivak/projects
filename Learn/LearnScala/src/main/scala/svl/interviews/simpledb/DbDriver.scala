package svl.interviews.simpledb

object DbDriver {
    def main(args: Array[String]) {
        val db = new MemoryDatabase[String, String] with DatabaseTransaction[String, String]
        var cont = true

        while(cont){
            try{
                val line = Console.readLine()
                line.split(' ').toList.map(_.trim).filter(!_.isEmpty) match {
                    case List("set", key, value) => db.set(key, value)
                    case List("get", key) => println(db.get(key).getOrElse("NULL"))
                    case List("count", value) => println(db.count(value.trim))
                    case List("delete", key) => db.delete(key.trim)
                    case List("begin") => db.beginTrans()
                    case List("commit") => db.commit()
                    case List("rollback") => db.rollback()
                    case List("end") => cont = false
                    case _ => println(s"Unknown command: $line")
                }
            }
            catch{
                case e:NoTransactionException => println("No Transaction")
            }
        }
    }
}
