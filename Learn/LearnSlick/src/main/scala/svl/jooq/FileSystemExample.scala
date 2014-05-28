package svl.jooq

import java.sql.DriverManager
import org.jooq._
import org.jooq.impl._
import org.jooq.impl.DSL._

object BoxUsers {
	def tableName = table("BoxUser")
	def userId = field("BoxUser.UserId")
	def name = field("BoxUser.UserName")
	def email = field("BoxUser.Email")
	def company = field("BoxUser.Company")
}

object BoxFiles {
	def tableName = table("BoxFile")

  def fileId = field("BoxFile.FileId")
  def name = field("BoxFile.FileName")
  def fileType = field("BoxFile.FileType")
  def ownerId = field("BoxFile.OwnerId")
}

object FileSystemExample {
	def main(args: Array[String]): Unit = {
    DriverManager.getDrivers
		val c =  DriverManager.getConnection("jdbc:mysql://localhost/vicTest", "victor", "vadim")
		val db = DSL.using(c, SQLDialect.MYSQL)

		val users = db.select(BoxUsers.userId, BoxUsers.name).from(BoxUsers.tableName)
		val files = db.select(BoxFiles.fileId, BoxFiles.name).from(BoxFiles.tableName)

		println(users.fetch())
    println("=====================================")
		println(files.fetch())
    println("=====================================")
    val query = db.select(BoxFiles.name, BoxUsers.name)
        .from(BoxUsers.tableName)
        .join(BoxFiles.tableName)
        .on(BoxUsers.userId.equal(BoxFiles.ownerId))
    println(query.fetch())
	}
}

