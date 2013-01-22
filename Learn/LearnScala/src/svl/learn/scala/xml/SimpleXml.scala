object SimpleXml {
    def main(args:Array[String]) {
        val xml1 = <a><b><c>hello</c></b></a>
        println(xml1 \ "b")
        println(xml1 \\ "c")

        val joe = <employee name="Joe" rank="code monkey"/>

        println(joe\"@name")
        println(joe\"@rank")

        val xml2 = <a><b>
            <employee name="Joe" rank="code monkey"/>
            <employee name="Vic" rank="loser"/>
        </b></a>

        println(xml2 \\ "employee")
    }
}


