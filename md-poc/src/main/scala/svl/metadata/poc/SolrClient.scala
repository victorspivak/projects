//package svl.metadata.poc
//
//import org.apache.solr.client.solrj.SolrServer
//import org.apache.solr.client.solrj.impl.HttpSolrServer
//import org.apache.solr.common.{SolrDocumentList, SolrInputDocument}
//import org.apache.solr.client.solrj.response.QueryResponse
//import org.apache.solr.common.params.ModifiableSolrParams
//
//object SolrClient extends App{
//  val solr: SolrServer = new HttpSolrServer("http://localhost:8080/solr")
//
//  for (i <- 0 to 10) {
//      indexDocument(solr, i+100, i)
//  }
//
//  val queryResult: QueryResponse = query(solr, "amount_d:[300 TO 700] AND vendor_s:BOX")
//  val results: SolrDocumentList = queryResult.getResults
//
//  import scala.collection.JavaConversions._
//
//  for (result <- results) {
//    System.out.printf("id:%s Vendor: %s Item %s --> Amount: %s\n",
//      result.getFieldValue("id"), result.getFieldValue("vendor_s"),
//      result.getFieldValue("item_s"), result.getFieldValue("amount_d"))
//  }
//
//  def indexDocument(solr: SolrServer, id: Int, i:Int) {
//    val document = new SolrInputDocument
//    document.addField("id", "id-%08d".format(id))
//    document.addField("amount_d", 100.0 * (i + 1))
//    document.addField("vendor_s", "SFDC")
//    document.addField("item_s", "Car")
//    solr.add(document)
//    solr.commit
//  }
//
//  private def query(solr: SolrServer, query: String): QueryResponse = {
//    val params = new ModifiableSolrParams
//    params.set("q", query)
//    solr.query(params)
//  }
//}
