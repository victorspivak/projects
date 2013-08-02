package svl.learn.kotlin

fun main(args : Array<String>) {
    println("Hello, world!")
    var str:String? = "Hi";
    println(greeting(str));
    str = null;
    str?.length();
    println(greeting(str));
}

fun greeting(name:String?):String {
    return "Hello " + name;
}
