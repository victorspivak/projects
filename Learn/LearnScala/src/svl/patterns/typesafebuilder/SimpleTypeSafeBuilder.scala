package svl.patterns.typesafebuilder

import scala.annotation.implicitNotFound

sealed trait TBoolean
sealed trait TTrue extends TBoolean
sealed trait TFalse extends TBoolean

class Builder[HasProperty <: TBoolean] private(i: Int) {
    protected def this() = this(-1)
    def withProperty(i: Int)(implicit ATTEMPT_TO_SET_PROPERTY_SECOND_TIME: HasProperty =:= TFalse) = new Builder[TTrue](i)

    def build(implicit THE_BUILDER_IS_NOT_FULLY_SPECIFIED_SEE_DOCUMENTATION: HasProperty =:= TTrue) = println(i)
}

object Builder { def apply() = new Builder[TFalse] }

object SimpleTypeSafeBuilder extends App {
    Builder().withProperty(2).build
//    Builder().withProperty(2).withProperty(4).build
//    Builder().build
}