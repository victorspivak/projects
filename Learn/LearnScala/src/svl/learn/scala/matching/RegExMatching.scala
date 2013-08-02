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
    }
}

