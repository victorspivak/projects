package svl.slick

import scala.slick.driver.MySQLDriver.simple._
import scala.slick.lifted

abstract class SlickEnumAsString extends Enumeration {
    implicit val enumMapper = MappedColumnType.base[Value, String](_.toString, this.withName)
}

abstract class SlickEnumAsInt extends Enumeration {
    implicit val enumMapper = MappedColumnType.base[Value, Int](_.id, this.apply)
}

object SlickUtils {
    def dumpQuery[T <: Table[R],R](q: lifted.Query[T, R])(implicit session: Session) : Unit = q.foreach(println)
}
