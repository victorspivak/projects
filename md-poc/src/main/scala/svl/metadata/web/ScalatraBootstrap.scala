package svl.metadata.web

import org.scalatra._
import javax.servlet.ServletContext

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    context.mount(new ObjectsRestServlet, "/objects/*")
    context.mount(new TypesRestServlet, "/types/*")
  }
}
