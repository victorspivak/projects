package svl.metadata.poc.md.database

import svl.metadata.poc.md.dictionary.MdTypes

class ObjectManager(db:Database) {
  def fetch(typeName:String, id:String) = {
    val session = db.connect
    val mdTypeOpt = MdTypes.getType(typeName)

    for {
      mdType <- mdTypeOpt
      obj <- session.fetch(id, mdType)
    } yield obj
  }
}

object ObjectManager {
  val db = MdDatabase()

  def apply() = new ObjectManager(db)
}
