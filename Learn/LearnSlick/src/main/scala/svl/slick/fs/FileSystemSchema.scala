package svl.slick.fs

import scala.slick.jdbc.meta.MTable
import java.sql.SQLException
import svl.slick.{SlickEnumAsInt, SlickEnumAsString}

object FileSystemSchema extends {
  val profile = scala.slick.driver.MySQLDriver
} with FileSystemSchema

case class UserId(id: Int) extends AnyVal

case class FileId(id: Int) extends AnyVal

trait FileSystemSchema {
  val profile: scala.slick.driver.JdbcProfile

  import profile.simple._

  lazy val ddl = boxUsers.ddl ++ boxFiles.ddl

  implicit val UserIdType = MappedColumnType.base[UserId, Int](_.id, new UserId(_))

  case class BoxUser(userId: UserId, name: String, email: String, company: Option[String] = None)

  case class BoxUserNameInfo(userId: UserId, name: String)

  class BoxUsers(tag: Tag) extends Table[BoxUser](tag, "BoxUser") {
    def userId = column[UserId]("UserId", O.PrimaryKey, O.AutoInc)

    def name = column[String]("UserName")

    def email = column[String]("Email")

    def company = column[String]("Company", O.Nullable)

    def * = (userId, name, email, company.?) <>(BoxUser.tupled, BoxUser.unapply)

    def nameInfo = (userId, name) <>(BoxUserNameInfo.tupled, BoxUserNameInfo.unapply)

    def ? = (userId.?, name.?, email.?, company.?)

    def nameIndex = index("ind_BoxUser_Name", name, unique = false)

    def emailIndex = index("ind_BoxUser_Email", email)
  }

  val boxUsers = TableQuery[BoxUsers]

  object BoxUsers {
    def insert(name: String, email: String, company: Option[String] = None)(implicit session: Session): UserId =
      insert(BoxUser(UserId(0), name, email, company))

    def insert(entity: BoxUser)(implicit session: Session): UserId =
      (boxUsers returning boxUsers.map(u => u.userId)) += entity

    def findById(id: UserId)(implicit session: Session) = boxUsers.filter(_.userId === id).firstOption

    def findByEmail(email: String)(implicit session: Session) = boxUsers.filter(_.email === email).firstOption
  }

  object FileType extends SlickEnumAsString() {
    //    object FileType extends SlickEnumAsInt() {
    type FileType = Value
    val Php = Value("Php")
    val Java = Value("Java")
    val Scala = Value("Scala")
  }

  implicit val FileIdType = MappedColumnType.base[FileId, Int](_.id, new FileId(_))

  case class BoxFile(fileId: FileId, name: String, fileType: FileType.Value, ownerId: UserId, vStamp: Int = 0)

  case class BoxFileEdit(vStamp: Int,
                         name: Option[String] = None, fileType: Option[FileType.Value] = None, ownerId: Option[UserId] = None)


  class BoxFiles(tag: Tag) extends Table[BoxFile](tag, "BoxFile") {
    def fileId = column[FileId]("FileId", O.PrimaryKey, O.AutoInc)

    def name = column[String]("FileName")

    def fileType = column[FileType.Value]("FileType")

    def ownerId = column[UserId]("OwnerId")

    def vStamp = column[Int]("vStamp")

    def * = (fileId, name, fileType, ownerId, vStamp) <>(BoxFile.tupled, BoxFile.unapply)

    def columns = List(vStamp.?, name.?, fileType.?, ownerId.?)

    def nameIndex = index("ind_BoxFile_Name", name, unique = false)

    def ownerIndex = index("ind_BoxFile_Owner", ownerId, unique = false)

    def fileTypeIndex = index("ind_BoxFile_Type", fileType, unique = false)
  }

  val boxFiles = TableQuery[BoxFiles]

  object BoxFiles {
    def insert(name: String, fileType: FileType.Value, ownerId: UserId)(implicit session: Session): FileId =
      insert(BoxFile(FileId(0), name, fileType, ownerId))

    def insert(entity: BoxFile)(implicit session: Session): FileId =
      (boxFiles returning boxFiles.map(u => u.fileId)) += entity

    def update(entity: BoxFile)(implicit session: Session) = {
      val q = for {f <- boxFiles if f.fileId === entity.fileId && f.vStamp === entity.vStamp} yield f

      println("============> " + q.updateStatement)
      val updateRes = q.update(entity.copy(vStamp = entity.vStamp + 1))
      println(s"Update Result: $updateRes")
    }

    //def columns = (boxFiles.vStamp, name.?, fileType.?, ownerId.?) <>(BoxFileEdit.tupled, BoxFileEdit.unapply)

    def update(fileId: FileId, entity: BoxFileEdit)(implicit session: Session) = {
//      val q = boxFiles.filter(f => f.fileId === entity.fileId && f.vStamp === entity.vStamp).map(x => (x.vStamp, x.name))
//      println("============> " + q.updateStatement)
//      q.update((entity.vStamp, entity.name.get))


//      val q = for {f <- boxFiles if f.fileId === entity.fileId && f.vStamp === entity.vStamp} yield f.->
//
//      println("============> " + q.updateStatement)
//      val updateRes = q.update(entity.copy(vStamp = entity.vStamp + 1))
//      println(s"Update Result: $updateRes")

      val q = boxFiles.filter(f => f.fileId === fileId && f.vStamp === entity.vStamp)

      var editColumns = List[Column[Object]]()
      var editColumnsValue = List[Object]()
      boxFiles.map{f=>
        val columns = f.columns
        editColumns = editColumns.+:(columns(0).asInstanceOf[Column[Object]])
        editColumnsValue = Some(entity.vStamp) :: editColumnsValue
        for (i <- 1 until entity.productArity){
          val value = entity.productElement(i).asInstanceOf[Option[Object]]
          if (value.isDefined){
            editColumns = editColumns.+:(columns(i).asInstanceOf[Column[Object]])
            editColumnsValue =  editColumnsValue.+:(value)
          }
        }
      }

      def toTuple[A <: Object](as:List[A]):Product = {
        val tupleClass = Class.forName("scala.Tuple" + as.size)
        tupleClass.getConstructors.apply(0).newInstance(as:_*).asInstanceOf[Product]
      }

      val columns = toTuple(editColumns.reverse)
      val values = toTuple(editColumnsValue.reverse)

      q.map(f=>columns)
      println(q.updateStatement)
      q.update(values)

    }

    def findById(id: FileId)(implicit session: Session) = boxFiles.filter(_.fileId === id).firstOption
  }

  def create(force: Boolean = false)(implicit session: Session) = {
    if (force)
      dropTables()

    if (!MTable.getTables().list().exists(t => t.name.name == boxUsers.baseTableRow.tableName)) {
      createTables()
      true
    }
    else
      false
  }

  def createTables()(implicit session: Session) {
    println("Creating File System schema")
    ddl.create
  }

  private def dropTables()(implicit session: Session) {
    println("Droping File System schema")
    try {
      ddl.drop
    }
    catch {
      case e: SQLException => //ignore
      case e: Exception => throw e
    }
  }

  def dumpDdl(): Unit = {
    println("Create tables:")
    ddl.createStatements.foreach(s => println(s"\t$s"))
    println("Drop tables:")
    ddl.dropStatements.foreach(s => println(s"\t$s"))
  }
}
