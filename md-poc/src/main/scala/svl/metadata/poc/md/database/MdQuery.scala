package svl.metadata.poc.md.database

import svl.metadata.poc.md.mdd.{MdAttributeRef, MdAttribute, MdType}
import scala.language.existentials

object MdQueryOperators{
  case class MdQueryOperator(ordinal:Int){}

  val Equal = MdQueryOperator(0)
  val NotEqual = MdQueryOperator(1)
  val Greater = MdQueryOperator(2)
  val GreaterEqual = MdQueryOperator(3)
  val Less = MdQueryOperator(4)
  val LessEqual = MdQueryOperator(5)
}
import MdQueryOperators._

object MdQueryBooleanOperators{
  case class MdQueryBooleanOperator(ordinal:Int){}

  val Empty = MdQueryBooleanOperator(0)
  val And = MdQueryBooleanOperator(1)
  val Or = MdQueryBooleanOperator(2)
}
import MdQueryBooleanOperators._

case class MdQueryConstrainRef[T](booleanOperator:MdQueryBooleanOperator, attrRef:MdAttributeRef[T], operator:MdQueryOperator, value:T){
  def toConstrain(mdType:MdType) = MdQueryConstrain(booleanOperator, mdType.getAttributeByName(attrRef.name).asInstanceOf[MdAttribute[T]], operator, value)
}

object MdQueryConstrainRef{
  def apply[T](attrRef:MdAttributeRef[T], operator:MdQueryOperator, value:T) = new MdQueryConstrainRef(Empty, attrRef, operator, value)
}

case class MdQueryConstrain[T](booleanOperator:MdQueryBooleanOperator, attribute:MdAttribute[T], operator:MdQueryOperator, value:T)

object MdSortingPolicy{
  case class MdSoringOrder(ordinal:Int){}

  val Ascending = MdSoringOrder(0)
  val Descending = MdSoringOrder(1)
}
import MdSortingPolicy._

case class MdSorting(attribute:MdAttribute[_], direction:MdSoringOrder)
case class MdQueryOptions(starting:Int, maxCount:Int)

case class MdQuery (ftQuery:String, mdType:MdType, constrains:List[MdQueryConstrain[_]], sorting:Option[MdSorting], options:MdQueryOptions){
}

class MdQueryBuilder(ftQuery:String, mdType:MdType) {
  var constrains = scala.collection.mutable.ArrayBuffer[MdQueryConstrain[_]]()
  var sorting:Option[MdSorting] = None
  var starting = 0
  var count = 10

  def filter(constrain:MdQueryConstrainRef[_]) = {
    constrains += constrain.toConstrain(mdType)
    this
  }

  def sortBy(attrRef:MdAttributeRef[_], direction:MdSoringOrder = Descending) = {
    sorting = Some(MdSorting(mdType.getAttributeByName(attrRef.name), direction))
    this
  }

  def startWith(starting:Int) = {
    this.starting = starting
    this
  }

  def maxCount(max:Int) = {
    this.count = max
    this
  }

  def build = new MdQuery(ftQuery, mdType, constrains.toList, sorting, MdQueryOptions(starting, count))
}

object MdQueryBuilder{
  def apply(query:String = "", mdType:MdType) = new MdQueryBuilder(query, mdType)
}

