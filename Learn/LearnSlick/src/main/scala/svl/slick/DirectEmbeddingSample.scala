package svl.slick

import scala.slick.driver.MySQLDriver.simple._
import scala.slick.direct._
import scala.slick.direct.AnnotationMapper._
import scala.slick.driver.MySQLDriver

// define classes
@table("COFFEES") case class Coffee(@column("COF_NAME") name: String, @column("PRICE") price: Double)

// side note: direct embedding not supported in singleton object until next Slick release.
class DirectEmbeddingSample {
    def query[T](q: QueryableValue[T])(implicit session: Session): T = backend.result(q, session)
    def query[T](q: Queryable[T])(implicit session: Session): Vector[T] = backend.result(q, session)

    // database queries specified using direct embedding
    val coffees = Queryable[Coffee]
    val priceAbove3 = coffees.filter(_.price > 3.0).map(_.name)
    val samePrice = for (
        c1 <- coffees;
        c2 <- coffees
        if c1.price == c2.price && c1 != c2
    )
    yield (c1.name, c2.name)

    // some dummy data
    val coffees_data = Vector(
        ("Colombian", 2),
        ("French_Roast", 2),
        ("Espresso", 5),
        ("Colombian_Decaf", 4),
        ("French_Roast_Decaf", 5)
    )

    // direct embedding backend (AnnotationMapper evaluates the @table and @column annotations. Use custom mapper for other mappings)
    val backend = new SlickBackend(MySQLDriver, AnnotationMapper)

    Database.forURL(Settings.dbUrl,
        driver = Settings.driver,
        user=Settings.user,
        password=Settings.password
    ) withSession { implicit session: Session =>
        //        createAndPopulateTable

        Seq(
            coffees,
            coffees.filter(_.price > 3.0).map(_.name), // inline query
            priceAbove3,
            samePrice
        ).foreach(q => println(query(q)))

        println(query(priceAbove3.length))
    }

    def createAndPopulateTable(implicit session: Session) {
        // insert using SQL (currently not supported by direct embedding)
        import slick.jdbc.StaticQuery.interpolation
        sqlu"create table COFFEES(COF_NAME varchar(255), PRICE DOUBLE)".execute
        (for {
            (name, sales) <- coffees_data
        } yield sqlu"insert into COFFEES values ($name, $sales)".first).sum
    }
}
