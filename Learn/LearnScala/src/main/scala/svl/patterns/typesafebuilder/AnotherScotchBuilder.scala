package svl.patterns.typesafebuilder

import scala.language.implicitConversions
import scala.language.reflectiveCalls

object BuilderPattern {
    sealed abstract class Preparation
    case object Neat extends Preparation
    case object OnTheRocks extends Preparation
    case object WithWater extends Preparation

    sealed abstract class Glass
    case object Short extends Glass
    case object Tall extends Glass
    case object Tulip extends Glass

    case class OrderOfScotch(brand:String, mode:Preparation, isDouble:Boolean, glass:Option[Glass])

    abstract class TRUE
    abstract class FALSE

    class ScotchBuilder[HB, HM, HD] (private[BuilderPattern] val theBrand:Option[String],
                                     private[BuilderPattern] val theMode:Option[Preparation],
                                     val theDoubleStatus:Option[Boolean],
                                     val theGlass:Option[Glass])            {
        def withGlass(g:Glass) = new ScotchBuilder[HB, HM, HD](theBrand, theMode, theDoubleStatus, Some(g))
    }

    implicit def enableWithBrand[HM, HD](builder:ScotchBuilder[FALSE, HM, HD]) = new {
        def withBrand(b:String) =
            new ScotchBuilder[TRUE, HM, HD](Some(b), builder.theMode, builder.theDoubleStatus, builder.theGlass)
    }

    implicit def enableWithMode[HB, HD](builder:ScotchBuilder[HB, FALSE, HD]) = new {
        def withMode(p:Preparation) =
            new ScotchBuilder[HB, TRUE, HD](builder.theBrand, Some(p), builder.theDoubleStatus, builder.theGlass)
    }

    implicit def enableDouble[HB, HM](builder:ScotchBuilder[HB, HM, FALSE]) = new {
        def isDouble(b:Boolean) =
            new ScotchBuilder[HB, HM, TRUE](builder.theBrand, builder.theMode, Some(b), builder.theGlass)
    }

    implicit def enableBuild(builder:ScotchBuilder[TRUE, TRUE, TRUE]) = new {
        def build() =
            new OrderOfScotch(builder.theBrand.get, builder.theMode.get, builder.theDoubleStatus.get, builder.theGlass)
    }

    def builder = new ScotchBuilder[FALSE, FALSE, FALSE](None, None, None, None)
}
import BuilderPattern._


















































































object AnotherScotchBuilder extends App{
    builder.withBrand("Mine").isDouble(b = true).withGlass(Tall).withMode(OnTheRocks).build()

	builder.withMode(OnTheRocks).isDouble(b = true).withBrand("aaa")
}





















