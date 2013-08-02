/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.servlet;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class HelloServlet extends HttpServlet {
    private String greeting = "Hello World";

    static {
        System.out.println("Hello");
    }

    public HelloServlet() {
    }

    public HelloServlet(String greeting) {
        this.greeting = greeting;
    }

    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        response.setContentType("text/html");
        System.out.println(">>>>>>>>>>>>>>>> start");
        try {
            Thread.sleep(5000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println(">>>>>>>>>>>>>>>> end");
        response.setStatus(HttpServletResponse.SC_OK);
        for (int i = 0; i < 1; i++) {
            response.getWriter().println("<h1>" + greeting + "</h1>");
        }
    }
}
