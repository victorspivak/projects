package svl.metadata.poc.md.database.solr

import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.impl.HttpSolrServer
import svl.metadata.poc.md.database._
import svl.metadata.poc.md.mdd._
import MdAttrDataTypes._
import MdQueryBooleanOperators._
import MdQueryOperators._
import org.apache.solr.common.{SolrDocument, SolrInputDocument}
import org.apache.solr.common.params.ModifiableSolrParams
import scala.collection.JavaConversions._
import scala.Some
import svl.metadata.poc.md.database.MdQuery
import svl.metadata.poc.md.database.DbObject
import svl.metadata.poc.md.mdd.MdAttribute
import scala.Some
import svl.metadata.poc.md.database.MdQuery
import svl.metadata.poc.md.database.DbObject

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

  val TypeField = "_obj_type_s"
}
import SolrHelper._

class SolrHelper(val solrEnv:SolrEnv){
  def indexDocument(dbObj:DbObject) {
    val mdType = dbObj.mdType
    val document = new SolrInputDocument
    document.addField("id", dbObj.id)
    document.addField(TypeField, dbObj.mdType.name)

    dbObj.values.foldLeft(document){(document, valueEntry) =>
      mdType.getAttributeByName(valueEntry._1) match { //svl filter out non-indexed fields
        case Some(attr:MdAttribute[_]) => document.addField(solrFieldName(attr), valueEntry._2)
          document
        case None => throw new UnexpectedStateException("Did not find attribute for specified value")
      }
    }

    solrEnv.solr.add(document)
//    solrEnv.solr.commit       //svl do we need commit here?
  }

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

    if (!buffer.isEmpty)
      buffer.append(" AND %s:%s AND ".format(TypeField, query.mdType.name))

    val result = constrains.foldLeft(buffer){(buffer, constrain) =>
      buffer.append(constrainExpression(constrain))
    }

    result.toString()
  }

  def solrDocumentToDbObject(document:SolrDocument, mdType:MdType) = {
    val builder = mdType.attributes.foldLeft(DbObjectBuilder(document.getFieldValue("id").asInstanceOf[String], mdType)) {(builder, attribute) =>
      builder.addAttribute(attribute.asInstanceOf[MdAttribute[Any]] -> document.getFieldValue(solrFieldName(attribute)).asInstanceOf[Any])
    }

    builder.build
  }

  def query(query:MdQuery) = {
    val mdType = query.mdType
    val queryStr = mdQueryToSolrQuery(query)
    println("Query: " + queryStr)

    val params = new ModifiableSolrParams
    params.set("q", queryStr)
    params.set("start", query.options.starting)
    val solrResults = solrEnv.solr.query(params)
    val resultList = solrResults.getResults

    resultList.foldLeft(List[DbObject]()){(list, solrDocument) =>
      solrDocumentToDbObject(solrDocument, mdType) :: list
    }
  }
}

