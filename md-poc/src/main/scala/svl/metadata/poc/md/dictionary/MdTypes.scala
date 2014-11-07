package svl.metadata.poc.md.dictionary

import svl.metadata.poc.md.mdd._


object MdTypes {
  val types:Map[String, GenericMdType] = MdTypesLoader.loadTypes.map{t => t.name.toLowerCase -> t}.toMap

  def getType(name:String) = types.get(name.toLowerCase)
}

object MdTypesLoader {
  def loadTypes = List(makeClaimType)

  def  makeClaimType = {
    val typeName = "Claim"
    val claimId = StringAttributeBuilder("ClaimId").build
    val insurance = StringAttributeBuilder("Insurance").build
    val amount = DoubleAttributeBuilder("Amount").doFiltering().build
    val item = StringAttributeBuilder("Item").doFiltering().build

    MdTypeBuilder(typeName).id(claimId).add(insurance).add(amount).add(item).
      doOptimisticLocking().use(MdIdGenerationPolicies.SeqIdPolicy, "ClaimID-%08d").build
  }
}