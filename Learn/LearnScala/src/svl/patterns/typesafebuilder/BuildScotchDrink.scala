package svl.patterns.typesafebuilder

object Scotch {
    sealed abstract class Preparation
    case object Neat extends Preparation
    case object OnTheRocks extends Preparation
    case object WithWater extends Preparation

    case class OrderOfScotch private[Scotch](brand:String, mode:Preparation, isDouble:Boolean)

    trait Option[+X]
    case class Some[+X](x:X) extends Option[X]{
        def get:X = x
    }
    trait None extends Option[Nothing] // this differs in the original implementation
    case object None extends None

    case class Builder[HAS_BRAND<:Option[String], HAS_MODE<:Option[Preparation], HAS_DOUBLE<:Option[Boolean]] private[Scotch]
    (brand:HAS_BRAND, mode:HAS_MODE, isDouble:HAS_DOUBLE) {
        def ~[X](f:Builder[HAS_BRAND,HAS_MODE,HAS_DOUBLE] => X):X = f(this)
    }

    def withBrand[M<:Option[Preparation],D<:Option[Boolean]](brand:String)(b:Builder[None,M,D]):Builder[Some[String],M,D] =
        Builder(Some(brand),b.mode,b.isDouble)
    def withMode[B<:Option[String],D<:Option[Boolean]](mode:Preparation)(b:Builder[B,None,D]):Builder[B,Some[Preparation],D] =
        Builder(b.brand,Some(mode),b.isDouble)
    def isDouble[B<:Option[String],M<:Option[Preparation]](isDouble:Boolean)(b:Builder[B,M,None]):Builder[B,M,Some[Boolean]] =
        Builder(b.brand,b.mode,Some(isDouble))

    def build(b:Builder[Some[String],Some[Preparation],Some[Boolean]]):OrderOfScotch =
        OrderOfScotch(b.brand.get,b.mode.get,b.isDouble.get)

    def builder:Builder[None,None,None] = Builder(None,None,None)
}

import Scotch._

object BuildScotchDrink extends App{
    val x:OrderOfScotch = builder ~ isDouble(true) ~ withMode(Neat) ~ withBrand("Blubber") ~ build
    //builder ~ isDouble(true) ~ withMode(Neat) ~ build // fails
    // builder ~ isDouble(true) ~ withMode(Neat) ~ withBrand("Blubber") ~ withBrand("Blubber") ~ build // fails

    println(x)
}
