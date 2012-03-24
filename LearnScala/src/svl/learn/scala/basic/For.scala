/*
 * User: Victor    Date: 1/21/12   Time: 1:29 AM
 */

val list = List(1,2,3,4,5,6,7,8,9)
for (i <- list)
    Console print (" " + i)
Console println ()

for (i <- 1 to 10)
    Console print (" " + i)
Console println ()

for (i <- 1 until 10)
    Console print (" " + i)
Console println ()

//with filter
for (i <- 2 to 100 by 2 if ((i % 3) == 0) || ((i % 7) == 3))
    Console print (" " + i)
Console println ()

//Nested loop
for {   i <- 1 to 10;
        j <- 1 to 10    
        if (i * j < 25)}
    Console print (" " + i + "/" + j)
Console println ()

def numbers = for { i <- 1 to 10;
        j <- 1 to 10
        if (i * j < 25)}
    yield i * j
Console println numbers

