package svl.metadata.poc.md.mdd

case class MdTypePolicy(optimisticLocking:Boolean, searchable:Boolean)

object MdIdGenerationPolicies{
  sealed case class MdIdGenerationPolicy()

  val RandomIdPolicy = MdIdGenerationPolicy()
  val SeqIdPolicy = MdIdGenerationPolicy()
  val SuppliedIdPolicy = MdIdGenerationPolicy()
}
import MdIdGenerationPolicies._

sealed case class IdGeneration(idColumn:MdAttribute, policy:MdIdGenerationPolicy, idTemplate:String)

class MdType(val id:String, val name:String, val attributes:List[MdAttribute], val idGeneration:IdGeneration){
  val attributesByName:Map[String, MdAttribute] =
    attributes.foldLeft(Map[String, MdAttribute]()) ((m:Map[String, MdAttribute], a:MdAttribute) => m + (a.name -> a))

  def getAttributeByName(attrName:String) = attributesByName.get(attrName)
  def idGenerationPolicy = idGeneration
  def optimisticLockingAttribute = getAttributeByName(MdType.OptimisticLockingColumnName)
}

object MdType {
  val OptimisticLockingColumnName = "OptLockingAttr"
}

class MdTypeBuilder (val name:String) {
  var attributes = scala.collection.mutable.ArrayBuffer[MdAttribute]()
  var attributesIndex = scala.collection.mutable.HashMap[String, MdAttribute]()
  var optimisticLocking = false
  var idColumn:MdAttribute = null
  var idTemplate = MdTypeBuilder.DefaultIdTemplate
  var idGenerationPolicy:MdIdGenerationPolicy = MdTypeBuilder.DefaultIdPolicy

  def add (attribute:MdAttribute):MdTypeBuilder = {
    if (attributesIndex.contains(attribute.name))
      throw MddExceptions.duplicateAttribute(name, attribute.name)
    if (attribute.name == MdType.OptimisticLockingColumnName)
      throw MddExceptions.invalidAttributeName(name, attribute.name)
    attributes += attribute
    attributesIndex += attribute.name -> attribute
    this
  }

  def id (attribute:MdAttribute):MdTypeBuilder = {
    if (idColumn != null)
      throw MddExceptions.duplicateIdAttribute(name, attribute.name, idColumn.name)
    this add attribute
    idColumn = attribute
    this
  }

  def doOptimisticLocking() = {
    this.optimisticLocking = true
    this
  }

  def use(idGenerationPolicy:MdIdGenerationPolicy, idTemplate:String = MdTypeBuilder.DefaultIdTemplate) = {
    this.idGenerationPolicy = idGenerationPolicy
    this.idTemplate = idTemplate
    this
  }

  def build = {
    def addOptimisticAttrIfNeeded(): List[MdAttribute] = {
      if (optimisticLocking)
        LongAttributeBuilder(MdType.OptimisticLockingColumnName).build :: attributes.toList
      else
        attributes.toList
    }

    if (idColumn == null)
      throw MddExceptions.missingIdColumn(name)

    new MdType("", name, addOptimisticAttrIfNeeded(), IdGeneration(idColumn, idGenerationPolicy, idTemplate))
  }
}

object MdTypeBuilder {
  val DefaultIdPolicy = SeqIdPolicy
  val DefaultIdTemplate = "id_%09d"
  implicit def mdAttributeBuilder2MdAttribute(builder:MdAttributeBuilder) = builder.build
  def apply (name:String) = new MdTypeBuilder(name)
}