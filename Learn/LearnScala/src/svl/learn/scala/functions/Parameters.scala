/*
 * User: Victor    Date: 1/24/12   Time: 12:14 AM
 */

def echo(args: String*) =
    for (arg <-args)
        println(arg)
        
echo ("Hello", "World")

val arr = Array("What's", "up", "doc?")
echo(arr: _*) //it is a special syntax to pass array in a place of variable arguments

//Named and default parameters
def speed(scale: Float = 1, distance: Float, time: Float): Float =
    scale * distance / time

Console println speed (1,100, 5)
Console println speed(distance = 100, time = 5)
Console println speed(time = 5, distance = 100)
Console println speed(time = 5, distance = 100, scale = 1000)

