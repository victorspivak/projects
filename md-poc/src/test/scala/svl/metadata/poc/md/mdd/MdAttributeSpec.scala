package svl.metadata.poc.md.mdd

import org.specs2._
import svl.metadata.poc.md.mdd.MdAttrDataTypes._

class MdAttributeSpec extends Specification {
  def is =
  "This is a specification to check Metadata Attributes"                            ^
                                                                                    p^
    "The StringAttribute with default values should"                                ^
    "named as '" + attrName + "'"                                                   ! attrNameFact(attrWithDefValues, attrName)  ^
    "have 'StringType' data type"                                                   ! attrTypeFact(attrWithDefValues, StringType)  ^
    "with empty id"                                                                 ! attrTypeFact(attrWithDefValues, StringType)  ^
    "with specified '-1' length"                                                    ! attrSizeFact(attrWithDefValues, -1)  ^
    "with filterable is 'false'"                                                    ! attrFilterableFact(attrWithDefValues, expected = false)  ^
    "with searchable is 'false'"                                                    ! attrSearchableFact(attrWithDefValues, expected = false)  ^
    "with compressing is 'false'"                                                   ! attrCompressingFact(attrWithDefValues, expected = false)  ^
    "with encrypting is 'false'"                                                    ! attrEncryptingFact(attrWithDefValues, expected = false)  ^
                                                                                    p^
    "The StringAttribute with specified values should"                              ^
    "named as '" + attrName + "'"                                                   ! attrNameFact(strAttr, attrName)  ^
    "have 'StringType' data type"                                                   ! attrTypeFact(strAttr, StringType)  ^
    "with specified '" + size32 + "' length"                                        ! attrSizeFact(strAttr, size32)  ^
    "with filterable is 'true'"                                                     ! attrFilterableFact(strAttr, expected = true)  ^
    "with searchable is 'true'"                                                     ! attrSearchableFact(strAttr, expected = true)  ^
    "with compressing is 'true'"                                                    ! attrCompressingFact(strAttr, expected = true)  ^
    "with encrypting is 'true'"                                                     ! attrEncryptingFact(strAttr, expected = true)  ^
                                                                                    p^
    "The attribute should have correct types"                                       ^
    StringAttributeBuilder.getClass.getSimpleName +
        " should specify 'StringType' data type"                                    ! attrTypeFact(StringAttributeBuilder("a").build, StringType)  ^
    IntAttributeBuilder.getClass.getSimpleName +
        " should specify 'IntegerType' data type"                                   ! attrTypeFact(IntAttributeBuilder("a").build, IntegerType)  ^
    DoubleAttributeBuilder.getClass.getSimpleName +
        " should specify 'DoubleType' data type"                                    ! attrTypeFact(DoubleAttributeBuilder("a").build, DoubleType)  ^
    DateAttributeBuilder.getClass.getSimpleName +
        " should specify 'DateType' data type"                                      ! attrTypeFact(DateAttributeBuilder("a").build, DateType)  ^
    LongAttributeBuilder.getClass.getSimpleName +
        " should specify 'LongType' data type"                                      ! attrTypeFact(LongAttributeBuilder("a").build, LongType)  ^
    end

  val size32 = 32
  val attrName = "MyAttr"

  def attrWithDefValues = StringAttributeBuilder(attrName).build
  def strAttr = StringAttributeBuilder(attrName, size32).doFiltering().doSearching().doCompressing().doEncrypting().build

  def attrIdFact(a:MdAttribute, expected:String) = a.id mustEqual expected
  def attrNameFact(a:MdAttribute, expected:String) = a.name mustEqual expected
  def attrTypeFact(a:MdAttribute, expected:MdAttrDataType[_]) = a.attrType.equals(expected)
  def attrSizeFact(a:MdAttribute, expected:Int) = a.size mustEqual expected
  def attrFilterableFact(a:MdAttribute, expected:Boolean) =  a.indexPolicy.filterable mustEqual expected
  def attrSearchableFact(a:MdAttribute, expected:Boolean) =  a.indexPolicy.searchable mustEqual expected
  def attrCompressingFact(a:MdAttribute, expected:Boolean) =  a.storePolicy.compressing mustEqual expected
  def attrEncryptingFact(a:MdAttribute, expected:Boolean) =  a.storePolicy.encrypting mustEqual expected
}
