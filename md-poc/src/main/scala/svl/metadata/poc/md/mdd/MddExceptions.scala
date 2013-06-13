package svl.metadata.poc.md.mdd

object MddExceptions {
  def duplicateAttribute(mdType:String, name:String) = new MddDuplicateAttributeException(mdType, name)
}

class MddUnknownTypeException(msg:String) extends Exception(msg)
class MddDuplicateAttributeException(mdType:String, name:String)
      extends Exception(String.format("Attribute %s already exist in the %s type.", name, mdType))

