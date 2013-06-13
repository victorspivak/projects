println("Hello")

println(string2Option(null))
println(string2Option("Hello"))

val s = "Hello"
val bytes = s.getBytes
val s1 = new String(bytes, "UTF8")

def string2Option(value:String) = value match {
    case null => None
    case _    => Some(value)
}