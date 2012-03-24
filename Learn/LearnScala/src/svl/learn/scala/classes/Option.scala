/*
 * User: Victor    Date: 2/1/12   Time: 1:30 AM
 */

val capitals = Map("France" -> "Paris", "Japan" -> "Tokyo")
val answer1 = capitals get  "France"
val answer2 = capitals get  "Russia"

def show(x: Option[String]) = x match {
    case Some(s) => s
    case None => "?"
}

println(answer1 + "   " + show(answer1))
println(answer2 + "   " + show(answer2))
