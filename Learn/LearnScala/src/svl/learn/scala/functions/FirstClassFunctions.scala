/*
 * User: Victor    Date: 1/24/12   Time: 12:01 AM
 */

var increase = (x:Int) => x + 1

Console println increase(10)

increase = (x:Int) => x * 2

Console println increase(10)

var incrementBy = 1
increase = (x:Int) => x + incrementBy
Console println increase(10)

incrementBy = 2
Console println increase(10)

