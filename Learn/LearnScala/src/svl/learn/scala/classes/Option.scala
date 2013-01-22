object Option {
    def main(args:Array[String]) {
        val capitals = Map("France" -> "Paris", "Japan" -> "Tokyo")
        val answer1 = capitals get  "France"
        val answer2 = capitals get  "Russia"

        def show(x: Option[String]) = x match {
            case Some(s) => s
            case None => "?"
        }

        println(answer1 + "   " + show(answer1))
        println(answer2 + "   " + show(answer2))

        def fd1(u:Option[String], p:Option[String], d:Option[String]) =
            for {   user <- u
                    password <- p
                    domain <- d}
            yield (domain + "/" + user + "/" + password)

        def fd2(u:Option[String], p:Option[String], d:Option[String]) = u.flatMap(user => p.flatMap(pass => d.map(domain => domain + "/" + user + "/" + pass)))

        def fd3(u:Option[String], p:Option[String], d:Option[String]) = u.flatMap(user => p.map(pass => d.getOrElse("SFDC-domain") + "/" + user + "/" + pass))

        println(fd1(Some("Vic"), Some("Pass"), Some("Domain")))
        println(fd1(Some("Vic"), Some("Pass"), None))
        println(fd2(Some("Vic"), Some("Pass"), Some("Domain")))
        println(fd2(Some("Vic"), Some("Pass"), None))
        println(fd3(Some("Vic"), Some("Pass"), Some("Domain")))
        println(fd3(Some("Vic"), Some("Pass"), None))
        println(fd3(Some("Vic"), None, None))
    }
}

