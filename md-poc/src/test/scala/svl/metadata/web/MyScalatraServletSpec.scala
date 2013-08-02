package svl.metadata.web

import org.scalatra.test.specs2._

class MyScalatraServletSpec extends ScalatraSpec { def is =
  "GET / on MyScalatraServlet"                     ^
    "should return status 200"                  ! root200^
    end

  addServlet(classOf[MyScalatraServlet], "/*")

  def root200 = get("/") {
    status must_== 200
  }
}
