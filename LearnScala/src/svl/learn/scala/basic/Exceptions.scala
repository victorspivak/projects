/*
 * User: Victor    Date: 1/26/12   Time: 12:07 AM
 */

try {
    val i = 5/0
} catch {
    case e:ArithmeticException => Console println "Expected exception even when asserts are disabled"
}


