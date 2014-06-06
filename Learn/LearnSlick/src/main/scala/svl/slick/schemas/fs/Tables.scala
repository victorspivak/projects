package svl.slick.schemas.fs
// AUTO-GENERATED Slick data model
/** Stand-alone Slick data model for immediate use */
object Tables extends {
  val profile = scala.slick.driver.MySQLDriver
} with Tables

/** Slick data model trait for extension, choice of backend or usage in the cake pattern. (Make sure to initialize this late.) */
trait Tables {
  val profile: scala.slick.driver.JdbcProfile
  import profile.simple._
  import scala.slick.model.ForeignKeyAction
  // NOTE: GetResult mappers for plain SQL are only generated for tables where Slick knows how to map the types of all columns.
  import scala.slick.jdbc.{GetResult => GR}
  
  /** DDL for all tables. Call .create to execute. */
  lazy val ddl = Boxfile.ddl ++ Boxuser.ddl ++ Coffees.ddl ++ Pet.ddl
  
  /** Entity class storing rows of table Boxfile
   *  @param fileid Database column FileId AutoInc, PrimaryKey
   *  @param filename Database column FileName 
   *  @param filetype Database column FileType 
   *  @param ownerid Database column OwnerId  */
  case class BoxfileRow(fileid: Int, filename: String, filetype: String, ownerid: Int)
  /** GetResult implicit for fetching BoxfileRow objects using plain SQL queries */
  implicit def GetResultBoxfileRow(implicit e0: GR[Int], e1: GR[String]): GR[BoxfileRow] = GR{
    prs => import prs._
    BoxfileRow.tupled((<<[Int], <<[String], <<[String], <<[Int]))
  }
  /** Table description of table BoxFile. Objects of this class serve as prototypes for rows in queries. */
  class Boxfile(tag: Tag) extends Table[BoxfileRow](tag, "BoxFile") {
    def * = (fileid, filename, filetype, ownerid) <> (BoxfileRow.tupled, BoxfileRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (fileid.?, filename.?, filetype.?, ownerid.?).shaped.<>({r=>import r._; _1.map(_=> BoxfileRow.tupled((_1.get, _2.get, _3.get, _4.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column FileId AutoInc, PrimaryKey */
    val fileid: Column[Int] = column[Int]("FileId", O.AutoInc, O.PrimaryKey)
    /** Database column FileName  */
    val filename: Column[String] = column[String]("FileName")
    /** Database column FileType  */
    val filetype: Column[String] = column[String]("FileType")
    /** Database column OwnerId  */
    val ownerid: Column[Int] = column[Int]("OwnerId")
    
    /** Index over (filename) (database name ind_BoxFile_Name) */
    val index1 = index("ind_BoxFile_Name", filename)
    /** Index over (ownerid) (database name ind_BoxFile_Owner) */
    val index2 = index("ind_BoxFile_Owner", ownerid)
    /** Index over (filetype) (database name ind_BoxFile_Type) */
    val index3 = index("ind_BoxFile_Type", filetype)
  }
  /** Collection-like TableQuery object for table Boxfile */
  lazy val Boxfile = new TableQuery(tag => new Boxfile(tag))
  
  /** Entity class storing rows of table Boxuser
   *  @param userid Database column UserId AutoInc, PrimaryKey
   *  @param username Database column UserName 
   *  @param email Database column Email 
   *  @param company Database column Company  */
  case class BoxuserRow(userid: Int, username: String, email: String, company: Option[String])
  /** GetResult implicit for fetching BoxuserRow objects using plain SQL queries */
  implicit def GetResultBoxuserRow(implicit e0: GR[Int], e1: GR[String], e2: GR[Option[String]]): GR[BoxuserRow] = GR{
    prs => import prs._
    BoxuserRow.tupled((<<[Int], <<[String], <<[String], <<?[String]))
  }
  /** Table description of table BoxUser. Objects of this class serve as prototypes for rows in queries. */
  class Boxuser(tag: Tag) extends Table[BoxuserRow](tag, "BoxUser") {
    def * = (userid, username, email, company) <> (BoxuserRow.tupled, BoxuserRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (userid.?, username.?, email.?, company).shaped.<>({r=>import r._; _1.map(_=> BoxuserRow.tupled((_1.get, _2.get, _3.get, _4)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column UserId AutoInc, PrimaryKey */
    val userid: Column[Int] = column[Int]("UserId", O.AutoInc, O.PrimaryKey)
    /** Database column UserName  */
    val username: Column[String] = column[String]("UserName")
    /** Database column Email  */
    val email: Column[String] = column[String]("Email")
    /** Database column Company  */
    val company: Column[Option[String]] = column[Option[String]]("Company")
    
    /** Index over (email) (database name ind_BoxUser_Email) */
    val index1 = index("ind_BoxUser_Email", email)
    /** Index over (username) (database name ind_BoxUser_Name) */
    val index2 = index("ind_BoxUser_Name", username)
  }
  /** Collection-like TableQuery object for table Boxuser */
  lazy val Boxuser = new TableQuery(tag => new Boxuser(tag))
  
  /** Entity class storing rows of table Coffees
   *  @param cofName Database column COF_NAME 
   *  @param price Database column PRICE  */
  case class CoffeesRow(cofName: Option[String], price: Option[Double])
  /** GetResult implicit for fetching CoffeesRow objects using plain SQL queries */
  implicit def GetResultCoffeesRow(implicit e0: GR[Option[String]], e1: GR[Option[Double]]): GR[CoffeesRow] = GR{
    prs => import prs._
    CoffeesRow.tupled((<<?[String], <<?[Double]))
  }
  /** Table description of table COFFEES. Objects of this class serve as prototypes for rows in queries. */
  class Coffees(tag: Tag) extends Table[CoffeesRow](tag, "COFFEES") {
    def * = (cofName, price) <> (CoffeesRow.tupled, CoffeesRow.unapply)
    
    /** Database column COF_NAME  */
    val cofName: Column[Option[String]] = column[Option[String]]("COF_NAME")
    /** Database column PRICE  */
    val price: Column[Option[Double]] = column[Option[Double]]("PRICE")
  }
  /** Collection-like TableQuery object for table Coffees */
  lazy val Coffees = new TableQuery(tag => new Coffees(tag))
  
  /** Entity class storing rows of table Pet
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param name Database column name 
   *  @param owner Database column owner 
   *  @param species Database column species 
   *  @param sex Database column sex 
   *  @param birth Database column birth 
   *  @param death Database column death  */
  case class PetRow(id: Int, name: Option[String], owner: Option[String], species: Option[String], sex: Option[String], birth: Option[java.sql.Date], death: Option[java.sql.Date])
  /** GetResult implicit for fetching PetRow objects using plain SQL queries */
  implicit def GetResultPetRow(implicit e0: GR[Int], e1: GR[Option[String]], e2: GR[Option[java.sql.Date]]): GR[PetRow] = GR{
    prs => import prs._
    PetRow.tupled((<<[Int], <<?[String], <<?[String], <<?[String], <<?[String], <<?[java.sql.Date], <<?[java.sql.Date]))
  }
  /** Table description of table pet. Objects of this class serve as prototypes for rows in queries. */
  class Pet(tag: Tag) extends Table[PetRow](tag, "pet") {
    def * = (id, name, owner, species, sex, birth, death) <> (PetRow.tupled, PetRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, name, owner, species, sex, birth, death).shaped.<>({r=>import r._; _1.map(_=> PetRow.tupled((_1.get, _2, _3, _4, _5, _6, _7)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column name  */
    val name: Column[Option[String]] = column[Option[String]]("name")
    /** Database column owner  */
    val owner: Column[Option[String]] = column[Option[String]]("owner")
    /** Database column species  */
    val species: Column[Option[String]] = column[Option[String]]("species")
    /** Database column sex  */
    val sex: Column[Option[String]] = column[Option[String]]("sex")
    /** Database column birth  */
    val birth: Column[Option[java.sql.Date]] = column[Option[java.sql.Date]]("birth")
    /** Database column death  */
    val death: Column[Option[java.sql.Date]] = column[Option[java.sql.Date]]("death")
  }
  /** Collection-like TableQuery object for table Pet */
  lazy val Pet = new TableQuery(tag => new Pet(tag))
}