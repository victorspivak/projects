object CaseClasses {
    def main(args:Array[String]) {
        abstract class Expr
        case class Var(name: String) extends Expr
        case class Number(num: Double) extends Expr
        case class UnOp(operator: String, arg: Expr) extends Expr
        case class BinOp(operator: String,
                         left: Expr, right: Expr) extends Expr

        def simplifyTop(expr: Expr): Expr = expr match {
            case UnOp("-", UnOp("-", e)) => e // Double negation
            case BinOp("+", e, Number(0)) => e // Adding zero
            case BinOp("*", e, Number(1)) => e // Multiplying by one
            case BinOp("+", x, y) if x == y => BinOp("*", x, Number(2)) //shows how to use guards. In this case we could specify that operands must be equal
            case _ => expr
        }

        val v = Var("x")
        val op = BinOp("+", Number(1), v)

        Console println  v
        Console println  op
        Console println  (simplifyTop(UnOp("-", UnOp("-", Var("x")))))

        val op1 = op match {
            case BinOp(op1, _, _) => op1
            case _ => null
        }

        println(op1)

        List(0, 1, 0) match {
            case List(0, _, _) => println("found it")
            case _ => println("NOT found it")
        }

        List(0, 1, 0, 0) match {
            case List(0, _, _) => println("found it")
            case _ => println("NOT found it")
        }

        List(0, 1, 0, 0) match {
            case List(0, _*) => println("found it")
            case _ => println("NOT found it")
        }

        //instead of class cast
        def generalSize(x: Any) = x match {
            case s: String => s.length
            case m: Map[_, _] => m.size
            case _ => 1
        }

        println (generalSize("Hello"))

        def generalSizeX(x: Any) = x match {
            case c: {def size : Int} => c.size
            case s: {def length : Int} => s.length
            case _ => 1
        }

        println (generalSize("Hello"))
        println (generalSize(List(1,2,3)))

        List(1,2,3).length
    }
}


