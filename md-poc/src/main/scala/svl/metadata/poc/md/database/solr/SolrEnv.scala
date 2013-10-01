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
import MdSortingPolicy._
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
    val solr: SolrServer = new HttpSolrServer("http://localhost:8983/solr")
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
    val document = makeSolrDocument(dbObj, mdType)

    solrEnv.solr.add(document, 100)
  }

  def indexDocument(dbObjs:List[DbObject], mdType:MdType) {
    solrEnv.solr.add(dbObjs.map(makeSolrDocument(_, mdType)), 100)
  }

  private def makeSolrDocument(dbObj: DbObject, mdType: MdType): SolrInputDocument = {
    val document = new SolrInputDocument
    document.addField("id", dbObj.id)
    document.addField(TypeField, dbObj.mdType.name)

    dbObj.values.foldLeft(document) {
      (document, valueEntry) =>
        mdType.getAttributeByName(valueEntry._1) match {
          case attr:MdAttribute[_] => if (attr.indexPolicy.filterable)
              document.addField(solrFieldName(attr), valueEntry._2)
            document
          case _ => document
        }
    }
    document
  }

  def solrFieldName(attribute:MdAttribute[_]) = attribute match {
    case MdAttribute(_, name, StringType, _, _, _) => name + "_s"
    case MdAttribute(_, name, IntegerType, _, _, _) => name + "_i"
    case MdAttribute(_, name, DoubleType, _, _, _) => name + "_d"
    case MdAttribute(_, name, DateType, _, _, _) => name + "_dt"
    case MdAttribute(_, name, LongType, _, _, _) => name + "_l"
    case _ => throw MddExceptions.unknownAttributeType(attribute)
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
    params.set("rows", query.options.maxCount)
    params.set("fl", "id")

    query.sorting.map{(sorting) => sorting match {
      case MdSorting(attribute, Ascending) => params.set("sort", solrFieldName(sorting.attribute) + " asc ")
      case MdSorting(attribute, Descending) => params.set("sort", solrFieldName(sorting.attribute) + " desc ")
    }}

    println("QueryParams: " + params)
    val solrResults = solrEnv.solr.query(params)
    val resultList = solrResults.getResults

    resultList.foldLeft(List[DbObject]()){(list, solrDocument) =>
      solrDocumentToDbObject(solrDocument, mdType) :: list
    }
  }
}

