package svl.learn.scala.basic

import util.parsing.json.JSON

object UnApply {
    def main(args:Array[String]) {
        class CC[T] {
            def unapply(a: Any): Option[T] = Some(a.asInstanceOf[T])
        }

        object M extends CC[Map[String, Any]]
        object L extends CC[List[Any]]
        object S extends CC[String]
        object D extends CC[Double]
        object I extends CC[Int] {
            override def unapply(a: Any): Option[Int] = Some(a.asInstanceOf[Double].toInt)
        }
        object B extends CC[Boolean]

        val vals = Map("k1" -> "val1", "k2" -> 2)
        val parsed = for {
            M(vs) <- Some(vals)
            S(s1) = vs("k1")
            i1 = vs("k2")
        } yield {
            (s1, i1)
        }

        println(parsed)

        val sss = S.unapply(vals("k1"))
        println(sss)

        val json = """ {"k1":"val1", "k2":2} """
        val parsed1 = JSON.parseFull(json)

        val v2 = for {
            M(js) <- parsed1
            S(s1) = js("k1")
            I(i1) = js("k2")
        } yield {
            (s1, i1)
        }

        println(v2)
    }
}
