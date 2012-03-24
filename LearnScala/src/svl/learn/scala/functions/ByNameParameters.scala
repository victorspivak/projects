/*
 * User: Victor    Date: 1/25/12   Time: 11:52 PM
 */

var assertionsEnabled = true
def myAssert(predicate: () => Boolean) =
    if (assertionsEnabled && !predicate())
        throw new AssertionError

myAssert(() => 5 > 3)

def byNameAssert(predicate: => Boolean) =
    if (assertionsEnabled && !predicate)
        throw new AssertionError
byNameAssert(5 > 3) //it looks much better than the previous example

//The following form from caller looks like the byNameAssert but the
//predicate evaluated during method invocation when byNameAssert it evaluates
//predicate when it is needed
def boolAssert(predicate: Boolean) =
    if (assertionsEnabled && !predicate)
        throw new AssertionError


assertionsEnabled = false
myAssert(() => 5/0 == 0)
byNameAssert(5/0 == 0)

try {
    boolAssert(5/0 == 0)
} catch {
    case e:ArithmeticException => Console println "Expected exception even when asserts are disabled"
}
