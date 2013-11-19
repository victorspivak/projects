object RegExMatching {
    def main(args:Array[String]) {
        val Email = """(.*)@(.*)""".r

        "victor.spivak@gmail.com" match {
            case Email(name, domain) => println(name + "  " + domain)
            case _ => println("Oops")
        }


        val toParse = "key ['value1', 'value2', 'value3']"
        val LineParser = """(.*)(\[.*\])""".r
        val ValParser = """\'([A-Za-z0-9]*)\'""".r
        toParse match {
            case LineParser(key, values) => {
                for (p <- ValParser findAllIn values) p match {
                    case ValParser(value) => println(value)
                    case _ => println("Oops 1")
                }

            }
            case _ => println("Oops")
        }


        val CommandTemplate = """\s*([a-zA-Z0-9]*)\s*([a-zA-Z0-9]*)\s*([a-zA-Z0-9]*).*""".r
        "set aaa bbb" match {
            case CommandTemplate(s1, s2, s3) => println(s"s1=>$s1 s2>=$s2 s3=>$s3")
        }
    }
}

