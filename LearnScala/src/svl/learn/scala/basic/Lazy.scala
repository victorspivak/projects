def func = {
    println("I am  in the func")
    true
}

println("before assigning normal val")
val a = func
println("after assigning normal val")
println(a)
println("after using normal val")

println("before assigning lazy val")
lazy val b = func
println("after assigning lazy val")
println(b)
println("after using lazy val")


