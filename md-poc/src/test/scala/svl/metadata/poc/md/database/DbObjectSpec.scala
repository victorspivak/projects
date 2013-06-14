package svl.metadata.poc.md.database

import org.specs2.Specification
import svl.metadata.poc.md.mdd.MdAttrDataTypes._
import svl.metadata.poc.md.mdd._

/*
 * User: victor    Date: 6/14/13   Time: 12:11 AM
 */
class DbObjectSpec extends Specification { def is =
  "This is a specification to check DbObject"                                       ^
                                                                                    p^
    "The simple test DbObject should "                                              ^
      "have 'My_ID'"                                                                ! (dbObj.id == id)  ^
      "have 'MyType' data type"                                                     ! (dbObj.mdType mustEqual simpleType)  ^
      "have the following attributes"                                               ^
        "String value 'Hello Attr 1' for 'Attr 1' attribute "                       ! (dbObj.getValue(attr1) == Some(attr1Val))  ^
        "Long value '100' for 'Attr 2' attribute "                                  ! (dbObj.getValue(attr2) ==  Some(attr2Val))  ^
        "Double value '3.14' for 'Attr 3' attribute "                               ! (dbObj.getValue(attr3) ==  Some(attr3Val))  ^
                                                                                    bt^
      "have the following attributes using getValue with class parameter"           ^
        "String value 'Hello Attr 1' for 'Attr 1' attribute "                       ! (dbObj.getValue(attr1Name)(classOf[String]) == Some(attr1Val))  ^
        "Long value '100' for 'Attr 2' attribute "                                  ! (dbObj.getValue(attr2Name)(classOf[Long]) ==  Some(attr2Val))  ^
        "Double value '3.14' for 'Attr 3' attribute "                               ! (dbObj.getValue(attr3Name)(classOf[Double]) ==  Some(attr3Val))  ^
                                                                                    p^
    "The DbObjectBuilder "                                                          ^
      "create object using name value pattern"                                      ^
        "String value 'Hello Attr 1' for 'Attr 1' attribute "                       ! (dbObjBuilderUsingNameValue.build.getValue(attr1Name)(classOf[String]) == Some(attr1Val))  ^
        "Long value '100' for 'Attr 2' attribute "                                  ! (dbObjBuilderUsingNameValue.build.getValue(attr2Name)(classOf[Long]) ==  Some(attr2Val))  ^
        "Double value '3.14' for 'Attr 3' attribute "                               ! (dbObjBuilderUsingNameValue.build.getValue(attr3Name)(classOf[Double]) ==  Some(attr3Val))  ^
                                                                                    bt^
      "create object using attribute value pattern"                                 ^
        "String value 'Hello Attr 1' for 'Attr 1' attribute "                       ! (dbObjBuilderUsingAttributeValue.build.getValue(attr1Name)(classOf[String]) == Some(attr1Val))  ^
        "Long value '100' for 'Attr 2' attribute "                                  ! (dbObjBuilderUsingAttributeValue.build.getValue(attr2Name)(classOf[Long]) ==  Some(attr2Val))  ^
        "Double value '3.14' for 'Attr 3' attribute "                               ! (dbObjBuilderUsingAttributeValue.build.getValue(attr3Name)(classOf[Double]) ==  Some(attr3Val))  ^
                                                                                    bt^
                                                                                    bt^
    "should not allow"                                                              ^
      "adding duplicate attributes using name value pattern"                        ! checkAddingDuplicateNameValue ^
      "adding unknown attributes using name value pattern"                          ! checkAddingUnknownNameValue ^
      "adding duplicate attributes using attribute value pattern"                   ! checkAddingDuplicateNameValue ^
      "adding unknown attributes using attribute value pattern"                     ! checkAddingUnknownNameValue ^
    end

  val typeName = "MyType"
  val attr1Name = "Attr 1"
  val attr2Name = "Attr 2"
  val attr3Name = "Attr 3"
  val idAttrName = "ID"
  val attr1Val = "Hello Attr 1"
  val attr2Val = 100L
  val attr3Val = 3.14
  val id = "My_ID"
  val attr1 = StringAttributeBuilder(attr1Name).build
  val attr2 = LongAttributeBuilder(attr2Name).build
  val attr3 = DoubleAttributeBuilder(attr3Name).build
  val idAttr = StringAttributeBuilder(idAttrName).build
  def simpleType = MdTypeBuilder(typeName).add(attr1).add(attr2).add(attr3).id(idAttr).build
  def values = Map(attr1Name -> attr1Val, attr2Name -> attr2Val, attr3Name -> attr3Val, idAttrName -> id)
  def dbObj = DbObject(id, simpleType, values)
  def dbObjBuilderUsingNameValue = DbObjectBuilder(simpleType).
    add(attr1Name -> attr1Val).add(attr2Name -> attr2Val).add(attr3Name -> attr3Val)
  def dbObjBuilderUsingAttributeValue =
    DbObjectBuilder(simpleType).addAttribute(attr1 -> attr1Val).addAttribute(attr2 -> attr2Val).addAttribute(attr3 -> attr3Val)

  def checkAddingDuplicateNameValue = dbObjBuilderUsingNameValue.add(attr1Name -> attr1Val) must throwAn[MddDuplicateAttributeException]
  def checkAddingUnknownNameValue = dbObjBuilderUsingNameValue.add("Bogus" -> attr1Val) must throwAn[MddUnknownAttributeException]
}
