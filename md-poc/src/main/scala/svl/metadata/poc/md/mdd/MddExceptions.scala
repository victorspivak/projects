package svl.metadata.poc.md.mdd

object MddExceptions {
  def duplicateAttribute(mdType:String, name:String) = new MddInvalidTypeBuildingException(mdType, "Attribute %s already exist.".format(name))
  def invalidAttributeName(mdType:String, name:String) = new MddInvalidTypeBuildingException(mdType, "Attribute %s may not be used.".format(name))
  def missingIdColumn(mdType:String) = new MddInvalidTypeBuildingException(mdType, "Missing ID Column.")
  def duplicateIdAttribute(mdType:String, existingId:String, name:String) =
      new MddInvalidTypeBuildingException(mdType, "Setting ID attribute \'%s\' but ID Attribute \'%s\' already assigned.".format(name, existingId))
}

class FeatureIsNotImplementedException(msg:String) extends Exception(msg + " is not implemented exception")
class MddUnknownTypeException(msg:String) extends Exception(msg)
class MddInvalidTypeBuildingException(mdType:String, msg:String)
      extends Exception(String.format("Building \'%s\' type: %s.", mdType, msg))

