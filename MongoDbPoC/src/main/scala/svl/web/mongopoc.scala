package svl.web

import org.scalatra._
import net.box.util.config.Config.SystemProperties
import net.box.util.service._
import net.box.util.service.metrics.OpenTSDBMetricsForwarding

import org.scalatra.ScalatraServlet
import org.scalatra.liftjson.JsonSupport

import com.yammer.metrics.HealthChecks
import com.yammer.metrics.core.HealthCheck
import com.yammer.metrics.core.HealthCheck._
import svl.mongo._
import net.box.util.config.Config
import net.box.Environment
import net.box.util.config.GlobalConfigGroup

import net.liftweb.json
import javax.servlet.http.HttpServletRequest
import net.liftweb.json.{DefaultFormats, JsonParser}
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonAST.JObject
import net.box.Environment
import net.box.util.config.GlobalConfigGroup
import net.liftweb.json.JsonAST.JString
import svl.scala.lib.PerformanceUtil

object MongoPocServiceMain extends ServiceMain
    with BoxServiceComponent
    with ProductionEnvironment // this gives me configEnv, props, context
    with OpenTSDBMetricsForwarding
    with ServletsComponent {

  case object MongoPocTest extends ServiceKind("net.box", "mongo-poc", "MNGO", 8085, 9085){}

  def kind = MongoPocTest

  def version = BuildInfo.version // from sbt-buildinfo plugin generated class

  def service = {
    val config = MongoPocConfig()
    val service = new MongoPocService(config)

    addServlet(ServletDef(new MongoPocServlet(service, config), "/mongo/*"))

    service
  }
}

case class MongoPocConfig(implicit env: Environment) {
 val config = Config(GlobalConfigGroup("mongopoc"))
 val name = config("mongopoc")("name").toString
 val dbName = config("mongopoc")("db-name").toString
 val collectionName = config("mongopoc")("collection-name").toString
}

class MongoPocService(config: MongoPocConfig)(implicit configEnv: Environment, context: ServiceContext) extends Service {
  val getsCounter = metrics.counter("GETs", tags = Map("source" -> "MongoPocService"))
  val getsTimer = metrics.timer("GET-TTFB") // Time-to-first-byte

  selfTest("cosmic rays shouldn't mess up bits") {
    require(1 == 1) // Throws if not true
    "all good"
  }

  HealthChecks.register(new HealthCheck("mongopoc") {
    def check = if (config.name == "mongo POC")
      Result.healthy else Result.unhealthy(new IllegalStateException("mongo poc from config should always be 'mongo POC'"))
  })

  def start() {
    logger.info("starting")

    // sys.error("BOOM!")
  }

  def stop() {
    logger.info("stopping")
  }
}

class MongoPocServlet(service: MongoPocService, config: MongoPocConfig)(implicit val configEnv: Environment, val context: ServiceContext, val props: SystemProperties)
  extends ScalatraServlet with JsonSupport {

  import net.liftweb.json.JsonDSL._

  get("/invoices/*") {
    service.getsTimer.time {
      service.getsCounter += 1

      if (requestPath.equals("/invoices/count")) {
        val res = PerformanceUtil.timerWithResult(MongoInvoiceService.count())

        ("db name" -> config.dbName) ~
        ("collection mame" -> config.collectionName) ~
        ("count" -> res._1) ~
        ("timing" -> res._2)
      }
      else if (requestPath.equals("/invoices/invoice")) {
        val filter = buildInvoiceQuery
        val res = PerformanceUtil.timerWithResult(MongoInvoiceService.query(filter))

        ("db name" -> config.dbName) ~
        ("collection mame" -> config.collectionName) ~
        ("query count" -> res._1) ~
        ("timing" -> res._2)
      }
      else {
        ("db name" -> config.dbName) ~
        ("collection mame" -> config.collectionName) ~
        ("Unknown Request" -> this.requestPath) ~
        ("count" -> this.request.getParameter("count"))
      }
    }
  }

  def buildInvoiceQuery = {
    val filter = new AbstractFilter()
    WebHelper.addStringFilter(request, filter, "vendor", Invoice.VENDOR)
    WebHelper.addIntFilter(request, filter, "amount", Invoice.AMOUNT)

    println("f" + filter.toDbObject)
    filter
  }

  post("/invoices/invoice") {
    implicit val formats = DefaultFormats

    val count = params.get("count").map(_.toInt).getOrElse(1)

    val res = PerformanceUtil.timerWithResult{
      val result = for{
        parsed <- JsonParser.parseOpt(request.getReader)
        inv <- parsed.extractOpt[Invoice]
      } yield inv

      result.map(MongoInvoiceService.createInvoices(count, _))
      result
    }

    ("db name" -> config.dbName) ~
    ("collection mame" -> config.collectionName) ~
    ("created invoice count" -> count) ~
    ("timing" -> res._2)
  }

  post("/invoices/batch") {
    implicit val formats = DefaultFormats

    val count       = params.get("count").map(_.toInt).getOrElse(1)
    val batch       = params.get("batch").map(_.toInt).getOrElse(1000)

    val res = PerformanceUtil.timer{MongoInvoiceService.createRandomInvoices(count, batch)(response.getOutputStream)}

    ("db name" -> config.dbName) ~
    ("collection mame" -> config.collectionName) ~
    ("created invoice count" -> count) ~
    ("batch size" -> batch) ~
    ("timing" -> res)
  }
}
