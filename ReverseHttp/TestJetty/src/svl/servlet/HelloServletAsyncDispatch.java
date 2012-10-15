/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.servlet;

import javax.servlet.*;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.text.MessageFormat;

@WebServlet(urlPatterns = "/hellodisp", asyncSupported = true)

public class HelloServletAsyncDispatch extends HttpServlet {
    private String greeting = "Hello Async Dispatch World";

    public HelloServletAsyncDispatch() {
    }

    public HelloServletAsyncDispatch(String greeting) {
        this.greeting = greeting;
    }

    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        HttpSession session = request.getSession();
        if (request.getDispatcherType() == DispatcherType.REQUEST) {
            final AsyncContext ctx = request.startAsync();

            ctx.setTimeout(300000);
            ctx.addListener(new AsyncListener() {
                public void onComplete(AsyncEvent event) throws IOException {
                    log("onComplete called");
                }

                public void onTimeout(AsyncEvent event) throws IOException {
                    log("onTimeout called");
                }

                public void onError(AsyncEvent event) throws IOException {
                    log("onError called");
                }

                public void onStartAsync(AsyncEvent event) throws IOException {
                    log("onStartAsync called");
                }
            });
            ctx.start(new Runnable() {
                public void run() {
                    ctx.dispatch();
                }
            });
        } else {
            final AsyncContext ctx = request.getAsyncContext();

            try {
                System.out.println(">>>>>>>>>>>>>>>> start");
                Thread.sleep(1000);
                System.out.println(">>>>>>>>>>>>>>>> end");
                response.getWriter().write(
                        MessageFormat.format("<h1>Dispatch Processing task in bgt_id:[{0}]</h1>",
                                Thread.currentThread().getId()));
            } catch (IOException e) {
                log("Problem processing task", e);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
//            ctx.complete();
        }

        System.out.println("*************** finish request");
    }
}
