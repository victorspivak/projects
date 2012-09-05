def dub (value:Int) = {
    2 * value
}

def dump (value:Int) {
    println (value)
}

dump(dub(5))

class Helper (var value:Int) {
    def mult (m:Int) = {
        value = m * value
    }

    def dump1 (comment:String) = {
        println (comment + " " + value)
        this
    }
}

val h = new Helper (10)
h mult 2
h dump1 "Result"


