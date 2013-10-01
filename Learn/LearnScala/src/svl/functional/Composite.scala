package svl.functional

object Composite {
    def main(args: Array[String]) {
        def composite1[A,B,C] (a:A, f:(A, B) => C) : (B) => C = (b:B) => f(a,b)

        def composite2[A,B,C](f:B => C, g:A => B) : A => C = (a:A) => f(g(a))
    }
}
