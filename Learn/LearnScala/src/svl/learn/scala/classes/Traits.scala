package svl.learn.scala.classes

object Traits {
    def main(args: Array[String]) {
        trait Db[K] {
            def insert(k:K)
            def delete(k:K)
        }

        trait Transaction[K] extends Db[K]{
            this:Db[K]=>

            abstract override def insert(k:K) {
                println("From Tr")
                super.insert(k)
            }
            abstract override def delete(k:K){
                println("From Tr")
                super.delete(k)
            }
        }

        class TestDb[K] extends Db[K]{
            def insert(k:K){println(s"Insert $k")}
            def delete(k:K){println(s"Delete $k")}
        }

        val db = new TestDb[String] with Transaction[String]
        db.insert("1")
    }
}
