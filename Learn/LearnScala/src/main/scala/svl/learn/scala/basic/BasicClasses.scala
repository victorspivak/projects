object BasicClasses extends App {
    val hello : Any = "Hello"
    Console println  hello.hashCode
    Console println  hello.##
    Console println  (hello ##)

    //val anyRef1:AnyRef = 5  //is compile error
    val anyVal1:AnyVal = 5
    //val anyVal2:AnyVal = "" //is compile error

}

