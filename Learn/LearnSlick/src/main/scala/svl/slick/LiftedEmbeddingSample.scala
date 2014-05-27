package svl.slick

import scala.slick.driver.MySQLDriver.simple._
import svl.slick.schemas.Tables._
import svl.slick.schemas.Tables

class LiftedEmbeddingSample {
    Database.forURL(Settings.dbUrl,
        driver = Settings.driver,
        user=Settings.user,
        password=Settings.password
    ) withSession { implicit session: Session =>
        SlickUtils.dumpQuery(selectWithConstrainExample1)
        println("----------------------------------------------")
        println(s"Total elements with price > 2: ${selectWithConstrainExample2.length.run}")
        println(s"Total prices with price > 2: ${selectWithConstrainExample2.map(_.price).sum.run}")
    }

    def selectWithConstrainExample1(implicit session: Session): Query[Tables.Coffees, Tables.CoffeesRow] = {
        val q = for {
            c <- Coffees
            if c.price > 2.0
        } yield c
        q
    }

    def selectWithConstrainExample2(implicit session: Session): Query[Tables.Coffees, Tables.CoffeesRow] = {
        val q = for {
            l <- Coffees.filter(_.price > 2.0)
        } yield l
        q
    }
}
