/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.servlet;

import javax.servlet.AsyncContext;
import javax.servlet.AsyncEvent;
import javax.servlet.AsyncListener;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.text.MessageFormat;

public class HelloServletAsync extends HttpServlet {
    private String greeting = "Hello Async World";

    public HelloServletAsync() {
    }

    public HelloServletAsync(String greeting) {
        this.greeting = greeting;
    }

    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // create the async context, otherwise getAsyncContext() will be null
        final AsyncContext ctx = request.startAsync();

        // set the timeout
        ctx.setTimeout(300000);

        // attach listener to respond to lifecycle events of this AsyncContext
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

        // spawn some task in a background thread
        ctx.start(new Runnable() {
            public void run() {

                try {
                    System.out.println(">>>>>>>>>>>>>>>> start");
                    Thread.sleep(10000);
                    System.out.println(">>>>>>>>>>>>>>>> end");
                    ctx.getResponse().getWriter().write(
                            MessageFormat.format("<h1>Processing task in bgt_id:[{0}]</h1>",
                                    Thread.currentThread().getId()));
                } catch (IOException e) {
                    log("Problem processing task", e);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }

                ctx.complete();
            }
        });

        System.out.println("*************** finish request");
    }
}
