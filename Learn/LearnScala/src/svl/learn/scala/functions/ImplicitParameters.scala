def func1(implicit par1:Int) {
    println(par1)
}

implicit val v1=2
func1(10)
func1

{//Case with def and no implicit
    def func2(par1:String) (implicit par2:String = "Def") = println(par1 + "  " + par2)
    func2 ("Imp")
    func2 ("Exp")("Wow")
}

{
    def func2(par1:String) (implicit par2:String = "Def") = println(par1 + "  " + par2)
    implicit val v2="It is me"
    func2 ("Imp")
    func2 ("Exp")("Wow")
}


