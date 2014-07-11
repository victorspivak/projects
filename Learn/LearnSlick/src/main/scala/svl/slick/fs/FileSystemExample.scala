package svl.slick.fs

import scala.slick.driver.MySQLDriver.simple._
import svl.slick.fs.FileSystemSchema._
import svl.slick.{SlickUtils, Settings}

object FileSystemExample {

    def populate()(implicit session: Session) : Unit = {
        val vicId   = BoxUsers.insert("Vic", "Vic@box.com")
        val johnId  = BoxUsers.insert("John", "John@box.com")
        val file1Id = BoxFiles.insert("file1.txt", FileType.Scala, vicId)
        val file2Id = BoxFiles.insert("file2.exe", FileType.Java, johnId)
        val file3Id = BoxFiles.insert("file3.php", FileType.Php, vicId)
        val file4Id = BoxFiles.insert("file4.java", FileType.Java, johnId)
        val file5Id = BoxFiles.insert("file5.php", FileType.Php, vicId)

        BoxFiles.insert("fake file1.php", FileType.Php, UserId(-1))

        SlickUtils.dumpQuery(boxUsers)
        SlickUtils.dumpQuery(boxFiles)
    }

    def create(force:Boolean = false)(implicit session: Session) : Unit = {
        //FileSystemSchema.dumpDdl()

        if (FileSystemSchema.create(force))
            populate()
    }

    lazy val db = Database.forURL(Settings.dbUrl,
        driver = Settings.driver,
        user=Settings.user,
        password=Settings.password
    )

    def run() = {
        db withSession { implicit session: Session =>
            create(force = true)

            val filesCount = boxFiles.length.run
            println(s"Files count $filesCount")

            val vic = boxUsers.filter(_.name === "Vic").firstOption
            println(vic)

            val crossJoin = for{
                u <- boxUsers
                f <- boxFiles
            } yield (f.name, u.name)
            crossJoin.foreach(println)

            println("========================================")
            val join1 = for{
                f <- boxFiles
                u <- boxUsers if u.userId === f.ownerId
            } yield (f.name, u.nameInfo)
            join1.foreach(println)

            println("========================================")
            val join2 = for{
                u <- boxUsers
                f <- boxFiles if u.userId === f.ownerId
            } yield (f.name, u.name)
            join2.foreach(println)

            println("========================================")
            val groupBy = for{
                u <- boxUsers if u.email === "Vic@box.com"
                f <- boxFiles if u.userId === f.ownerId
            } yield f

            groupBy.groupBy(_.fileType).map { case (id, file) => (id, file.length)}.foreach(println)

            println("========================================")
            val user1 = BoxUsers.findByEmail("Vic@box.com")
            user1.map(u => println(BoxUsers.findById(u.userId)))

            println("========================================")
            val outerJoin1 = for{
                (f, u) <- boxFiles leftJoin boxUsers on (_.ownerId === _.userId)
            } yield (f, u.name.?)
            outerJoin1.foreach(println)

            println("========================================")
            val file1 = BoxFiles.findById(FileId(1))
            println(file1)
            file1.map{f =>
//                val updated = f.copy(name = "file1 updated.txt")
//                BoxFiles.update(updated)
//                BoxFiles.update(updated)
//                val file2 = BoxFiles.findById(updated.fileId)
//                file2.map(f=>BoxFiles.update(f.copy(fileType = FileType.Java)))
              val updated = BoxFileEdit(f.vStamp, name = Some("file1 updated.txt"))
              BoxFiles.update(f.fileId, updated)
              BoxFiles.update(f.fileId, updated)
              val file2 = BoxFiles.findById(f.fileId)
              file2.map(f=>BoxFiles.update(f.copy(fileType = FileType.Java)))

            }
        }
    }
}
