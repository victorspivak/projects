package svl.metadata.poc.md.mdd

import org.specs2.Specification
import MdTypeBuilder._
import MdIdGenerationPolicies._

class MdTypeSpec extends Specification { def is =
  "This is a specification to check Metadata Type"                                  ^
                                                                                    p^
    "The simple type with 3 attributes should"                                      ^
    "named as '" + typeName + "'"                                                   ! typeNameFact(makeSimpleType, typeName)  ^
    "with empty id"                                                                 ! typeIdFact(makeSimpleType, "")  ^
    "have attribute with '" + attr1Name + "' name"                                  ! typeAttrFact(makeSimpleType, attr1Name, attr1)  ^
    "have attribute with '" + attr2Name + "' name"                                  ! typeAttrFact(makeSimpleType, attr2Name, attr2)  ^
    "have attribute with '" + attr3Name + "' name"                                  ! typeAttrFact(makeSimpleType, attr3Name, attr3)  ^
    "and attribute '" + attr2Name + "' is ID Column"                                ! typeIdAttrFact(makeSimpleType, attr2Name, attr2)  ^
    "and default id generation policy is sequential"                                ! idGenerationPolicyFact(makeSimpleType, SeqIdPolicy)  ^
    "does not have attribute with bogus name"                                       ! typeAttrFact(makeSimpleType, "bogus", null)  ^
    "and containAttribute returns true for existing one"                            ! makeSimpleType.containsAttribute(attr1Name)  ^
    "and containAttribute returns false for bogus one"                              ! !makeSimpleType.containsAttribute("Bogus")  ^
                                                                                    p^
    "The type builder should "                                                      ^
    "create"                                                                        ^
      "type with optimistic locking attr when it is specified"                      ! typeOptimisticLockingFact(makeSimpleTypeBuilder.doOptimisticLocking().build, expected = true) ^
      "type without optimistic locking attr when it is not specified"               ! typeOptimisticLockingFact(makeSimpleTypeBuilder.build, expected = false) ^
      "type with RandomIdPolicy id generation policy when it was specified"         ! idGenerationPolicyFact(makeSimpleTypeBuilder.use(RandomIdPolicy).build, RandomIdPolicy)^
      "type with SeqIdPolicy id generation policy when it was specified"            ! idGenerationPolicyFact(makeSimpleTypeBuilder.use(SeqIdPolicy).build, SeqIdPolicy)^
      "type with SuppliedIdPolicy id generation policy when it was specified"       ! idGenerationPolicyFact(makeSimpleTypeBuilder.use(SuppliedIdPolicy).build, SuppliedIdPolicy)^
      "type with default \'" + MdTypeBuilder.DefaultIdTemplate + "\'id template when it was not specified" ! idTemplateFact(makeSimpleTypeBuilder.use(SeqIdPolicy).build, MdTypeBuilder.DefaultIdTemplate)^
      "type with default \'%s\'id template when it was specified"                   ! idTemplateFact(makeSimpleTypeBuilder.use(SeqIdPolicy, "%s").build, "%s") ^
                                                                                    bt^
    "allow "                                                                        ^
      "adding attribute builders for convenience"                                   ! addingAttrBuilderFact(makeSimpleTypeBuilder)  ^
                                                                                    bt^
    "not allow"                                                                     ^
      "adding duplicate attributes"                                                 ! dupAttrFact(makeSimpleTypeBuilder, attr1)  ^
      "building type w/o id column throws exception"                                ! typeWitoutIdFact  ^
    end

  val typeName = "MyType"
  val attr1Name = "Attr 1"
  val attr2Name = "Attr 2"
  val attr3Name = "Attr 3"
  val attr4Name = "Attr 4"
  val attr1 = StringAttributeBuilder(attr1Name).build
  val attr2 = StringAttributeBuilder(attr2Name).build
  val attr3 = StringAttributeBuilder(attr3Name).build
  def makeSimpleTypeBuilder = MdTypeBuilder(typeName).add(attr1).id(attr2).add(attr3)
  def makeSimpleType = makeSimpleTypeBuilder.build

  def typeIdFact(t:MdType, expected:String) = t.id mustEqual expected
  def typeNameFact(t:MdType, expected:String) = t.name mustEqual expected
  def typeAttrFact(t:MdType, attrName:String, expected:MdAttribute[_]) = t.getAttributeByNameOpt(attrName) === Option(expected)
  def typeOptimisticLockingFact(t:MdType, expected:Boolean) = t.optimisticLockingAttribute.isDefined === expected
  def idGenerationPolicyFact(t:MdType, expected:MdIdGenerationPolicy) = t.idGenerationPolicy.policy === expected
  def idTemplateFact(t:MdType, expected:String) = t.idGenerationPolicy.idTemplate === expected

  def typeIdAttrFact(t:MdType, attrName:String, expected:MdAttribute[_]) = t.idGenerationPolicy.idColumn == expected
  def dupAttrFact(tb:MdTypeBuilder, attr1:MdAttribute[_]) = tb.add(attr1) must throwAn[MddInvalidTypeBuildingException]
  def typeWitoutIdFact = MdTypeBuilder(typeName).add(attr1).build must throwAn[MddInvalidTypeBuildingException]
  def addingAttrBuilderFact(tb:MdTypeBuilder) = {
    val mdType = tb.add(StringAttributeBuilder(attr4Name)).build
    val expectedAttribute: MdAttribute[_] = StringAttributeBuilder(attr4Name).build
    mdType.getAttributeByName(attr4Name) === expectedAttribute
  }
}
