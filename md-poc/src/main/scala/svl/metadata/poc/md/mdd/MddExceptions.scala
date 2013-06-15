package svl.metadata.poc.md.mdd

object MddExceptions {
  def duplicateAttribute(mdType:String, name:String) = new MddInvalidTypeBuildingException(mdType, "Attribute %s already exist.".format(name))
  def duplicateAttributeValue(name:String) = new MddDuplicateAttributeException("Attribute %s already exist.".format(name))
  def invalidAttributeName(mdType:String, name:String) = new MddInvalidTypeBuildingException(mdType, "Attribute %s may not be used.".format(name))
  def missingIdColumn(mdType:String) = new MddInvalidTypeBuildingException(mdType, "Missing ID Column.")
  def unknownAttribute(mdType:String, name:String) = new MddUnknownAttributeException("Type \'%s\' does not have \'%s\' attribute.".format(mdType, name))
  def duplicateIdAttribute(mdType:String, existingId:String, name:String) =
      new MddInvalidTypeBuildingException(mdType, "Setting ID attribute \'%s\' but ID Attribute \'%s\' already assigned.".format(name, existingId))
}

class MddBaseException(message:String = "", cause:Throwable = null) extends Exception(message, cause)
class FeatureIsNotImplementedException(msg:String) extends MddBaseException(msg + " is not implemented exception")
class UnexpectedStateException(msg:String) extends MddBaseException(msg)

class MddUnknownTypeException(msg:String) extends MddBaseException(msg)
class MddDuplicateAttributeException(msg:String) extends MddBaseException(msg)
class MddUnknownAttributeException(msg:String) extends MddBaseException(msg)
class MddInvalidTypeBuildingException(mdType:String, msg:String)
      extends MddBaseException(String.format("Building \'%s\' type: %s.", mdType, msg))

