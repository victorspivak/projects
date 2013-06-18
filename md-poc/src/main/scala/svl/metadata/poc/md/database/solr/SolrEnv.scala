package svl.metadata.poc.md.database.solr

import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.impl.HttpSolrServer
import svl.metadata.poc.md.database.{MdQueryOperators, MdQueryBooleanOperators, MdQueryConstrain, MdQuery}
import svl.metadata.poc.md.mdd.{MdAttrDataTypes, MdAttribute}
import MdAttrDataTypes._
import MdQueryBooleanOperators._
import MdQueryOperators._

trait SolrEnv {
  def solr: SolrServer
  def helper = new SolrHelper(this)
}

trait DefaultSolrEnv{
  def solrEnv:SolrEnv = new SolrEnv{
    val solr: SolrServer = new HttpSolrServer("http://localhost:8080/solr")
  }
}

object SolrHelper{
  val AndOperator = " AND "
  val OrOperator = " OR "
  val NotOperator = " NOT "
}
import SolrHelper._

class SolrHelper(val solrEnv:SolrEnv){
  def solrFieldName(attribute:MdAttribute[_]) = attribute match {
    case MdAttribute(_, name, StringType, _, _, _) => name + "_s"
    case MdAttribute(_, name, IntegerType, _, _, _) => name + "_i"
    case MdAttribute(_, name, DoubleType, _, _, _) => name + "_d"
    case MdAttribute(_, name, DateType, _, _, _) => name + "_dt"
    case MdAttribute(_, name, LongType, _, _, _) => name + "_l"
  }

  def startConstrainExpression(constrain:MdQueryConstrain[_]) = constrain match {
    case MdQueryConstrain(Empty, _, _, _) => ""
    case MdQueryConstrain(And, _, _, _) => AndOperator
    case MdQueryConstrain(Or, _, _, _) => OrOperator
  }

  def valueExpression(constrain:MdQueryConstrain[_]) = {
    constrain match{
      case MdQueryConstrain(_, attribute, Equal, value) =>
        " %s:%s ".format(solrFieldName(attribute), constrain.value.toString)
      case MdQueryConstrain(_, attribute, NotEqual, value) =>
        " NOT %s:%s ".format(solrFieldName(attribute), constrain.value.toString)
      case MdQueryConstrain(_, attribute, Greater, value) =>
        " %s:{%s TO *] ".format(solrFieldName(attribute), constrain.value.toString)
      case MdQueryConstrain(_, attribute, GreaterEqual, value) =>
        " %s:[%s TO *] ".format(solrFieldName(attribute), constrain.value.toString)
      case MdQueryConstrain(_, attribute, Less, value) =>
        " %s:[* TO %s} ".format(solrFieldName(attribute), constrain.value.toString)
      case MdQueryConstrain(_, attribute, LessEqual, value) =>
        " %s:[* TO %s] ".format(solrFieldName(attribute), constrain.value.toString)
    }
  }

  def constrainExpression(constrain:MdQueryConstrain[_]) = {
    startConstrainExpression(constrain) + valueExpression(constrain)
  }

  def mdQueryToSolrQuery(query:MdQuery) = {
    val buffer = new StringBuilder(query.ftQuery)
    val constrains: List[MdQueryConstrain[_]] = query.constrains

    if (!buffer.isEmpty && !constrains.isEmpty)
      buffer.append(AndOperator)

    val constrainStr = constrains.foldLeft(buffer){(buffer, constrain) =>
      buffer.append(constrainExpression(constrain))
    }

    buffer.toString()
  }
}

