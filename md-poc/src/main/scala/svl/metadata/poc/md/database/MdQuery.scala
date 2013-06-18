package svl.metadata.poc.md.database

import svl.metadata.poc.md.mdd.{MdAttribute, MdType}

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


case class MdQueryConstrain[T](booleanOperator:MdQueryBooleanOperator, attribute:MdAttribute[T], operator:MdQueryOperator, value:T)

object MdQueryConstrain{
  def apply[T](attribute:MdAttribute[T], operator:MdQueryOperator, value:T) = new MdQueryConstrain(Empty, attribute, operator, value)
}

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

  def filter(constrain:MdQueryConstrain[_]) = {
    constrains += constrain
    this
  }

  def sortBy(attribute:MdAttribute[_], direction:MdSoringOrder = Descending) = {
    sorting = Some(MdSorting(attribute, direction))
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

