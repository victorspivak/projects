package svl.metadata.poc.md.mdd

class MdType(val id:String, val name:String, val attributes:List[MdAttribute]){
  val attributesByName:Map[String, MdAttribute] =
    attributes.foldLeft(Map[String, MdAttribute]()) ((m:Map[String, MdAttribute], a:MdAttribute) => m + (a.name -> a))

  def getAttributeByName(attrName:String) = attributesByName.get(attrName)
}

class MdTypeBuilder (val name:String) {
  var attributes = scala.collection.mutable.ArrayBuffer[MdAttribute]()
  var attributesIndex = scala.collection.mutable.HashMap[String, MdAttribute]()

  def + (attribute:MdAttribute):MdTypeBuilder = {
    if (attributesIndex.contains(attribute.name))
      throw MddExceptions.duplicateAttribute(name, attribute.name)
    attributes += attribute
    attributesIndex += attribute.name -> attribute
    this
  }

  def + (attributeBuilder:AttributeBuilder):MdTypeBuilder = {
    this + attributeBuilder.build
  }

  def build = new MdType("", name, attributes.toList)
}

object MdTypeBuilder {
  def apply (name:String) = new MdTypeBuilder(name)
}