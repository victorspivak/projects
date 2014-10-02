package svl.metadata.web

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ DefaultServlet, ServletContextHandler }
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener

object JettyServer {
  def main(args: Array[String]) {
    try {
      val port = if (System.getenv("PORT") != null) System.getenv("PORT").toInt else 8180

      val server = new Server(port)
      val context = new WebAppContext()
      context setContextPath "/"
      context.setInitParameter(ScalatraListener.LifeCycleKey, "svl.metadata.web.ScalatraBootstrap")
      context.setResourceBase("src/main/webapp")
      context.addEventListener(new ScalatraListener)
      context.addServlet(classOf[DefaultServlet], "/")

      server.setHandler(context)

      server.start()
      server.join()
    }
    catch {
      case e:Exception => println(e)
        System.exit(-1)
    }
  }
}
