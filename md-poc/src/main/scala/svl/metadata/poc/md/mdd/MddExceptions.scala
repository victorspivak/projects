package svl.metadata.poc.md.mdd

import svl.metadata.poc.md.database.DbObject
import svl.metadata.poc.md.mdd.MdAttrDataTypes.MdAttrDataType

object MddExceptions {
  def duplicateAttribute(mdType:String, name:String) = new MddInvalidTypeBuildingException(mdType, "Attribute %s already exist.".format(name))
  def duplicateAttributeValue(name:String) = new MddDuplicateAttributeException("Attribute %s already exist.".format(name))
  def invalidAttributeName(mdType:String, name:String) = new MddInvalidTypeBuildingException(mdType, "Attribute %s may not be used.".format(name))
  def missingIdColumn(mdType:String) = new MddInvalidTypeBuildingException(mdType, "Missing ID Column.")
  def unknownAttribute(mdType:String, name:String) = new MddUnknownAttributeException("Type \'%s\' does not have \'%s\' attribute.".format(mdType, name))
  def unknownAttributeType(mdAttribute:MdAttribute[_]) = new MddUnknownAttributeTypeException("Unknown Attribute type for \'%s\'".format(mdAttribute.toString))
  def unknownAttributeType(mdAttributeType:MdAttrDataType[_]) = new MddUnknownAttributeTypeException("Unknown Attribute type \'%s\'".format(mdAttributeType.toString))
  def duplicateIdAttribute(mdType:String, existingId:String, name:String) =
      new MddInvalidTypeBuildingException(mdType, "Setting ID attribute \'%s\' but ID Attribute \'%s\' already assigned.".format(name, existingId))
  def missingOptimisticLockingAttribute(dbObj:DbObject) =
    new MddMissingOptimisticLockingAttributeException("Object id \'%s\' of type \'%s\' does not have optimistic lock attribute.".format(dbObj.id, dbObj.mdType.name))
  def concurrentObjectUpdate(dbObj:DbObject) =
    new MddConcurrentObjectUpdateException("Updating object id \'%s\' of type \'%s\' -- concurrent update.".format(dbObj.id, dbObj.mdType.name))
}

class MddBaseException(message:String = "", cause:Throwable = null) extends Exception(message, cause)
class FeatureIsNotImplementedException(msg:String) extends MddBaseException(msg + " is not implemented exception")
class UnexpectedStateException(msg:String) extends MddBaseException(msg)

class MddUnknownTypeException(msg:String) extends MddBaseException(msg)
class MddUnknownAttributeTypeException(msg:String) extends MddBaseException(msg)
class MddDuplicateAttributeException(msg:String) extends MddBaseException(msg)
class MddUnknownAttributeException(msg:String) extends MddBaseException(msg)
class MddMissingOptimisticLockingAttributeException(msg:String) extends MddBaseException(msg)
class MddConcurrentObjectUpdateException(msg:String) extends MddBaseException(msg)

class MddInvalidTypeBuildingException(mdType:String, msg:String)
      extends MddBaseException(String.format("Building \'%s\' type: %s.", mdType, msg))

