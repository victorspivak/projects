package svl.metadata.poc.md.mdd

import org.specs2.Specification

class MdTypeSpec extends Specification {
  def is =
  "This is a specification to check Metadata Type"                                  ^
                                                                                    p^
    "The simple type with 3 attributes should"                                      ^
    "named as '" + typeName + "'"                                                   ! typeNameFact(makeSimpleType, typeName)  ^
    "with empty id"                                                                 ! typeIdFact(makeSimpleType, "")  ^
    "have attribute with '" + attr1Name + "' name"                                  ! typeAttrFact(makeSimpleType, attr1Name, attr1)  ^
    "have attribute with '" + attr2Name + "' name"                                  ! typeAttrFact(makeSimpleType, attr2Name, attr2)  ^
    "have attribute with '" + attr3Name + "' name"                                  ! typeAttrFact(makeSimpleType, attr3Name, attr3)  ^
    "does not have attribute with bogus name"                                       ! typeAttrFact(makeSimpleType, "bogus", null)  ^
                                                                                    p^
    "The type builder should "                                                      ^
    "not allow adding duplicate attributes"                                         ! dupAttrFact(makeSimpleTypeBuilder, attr1)  ^
    end

  val typeName = "MyType"
  val attr1Name = "Attr 1"
  val attr2Name = "Attr 2"
  val attr3Name = "Attr 3"
  val attr1 = StringAttributeBuilder(attr1Name).build
  val attr2 = StringAttributeBuilder(attr2Name).build
  val attr3 = StringAttributeBuilder(attr3Name).build
  def makeSimpleTypeBuilder = MdTypeBuilder(typeName) + attr1 + attr2 + attr3
  def makeSimpleType = makeSimpleTypeBuilder.build

  def typeIdFact(t:MdType, expected:String) = t.id mustEqual expected
  def typeNameFact(t:MdType, expected:String) = t.name mustEqual expected
  def typeAttrFact(t:MdType, attrName:String, expected:MdAttribute) = t.getAttributeByName(attrName) === Option(expected)
  def dupAttrFact(tb:MdTypeBuilder, attr1:MdAttribute) = tb + attr1 must throwAn[MddDuplicateAttributeException]
}
