val r = """(.*\n)""".r
val input = "Hello  world\n"
input match {
  case r(s) => println(s)
  case _ => println("Oops")
}

val results = r.findAllIn(input)
results.toList

