/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.servlet.reversehttp;

import javax.servlet.AsyncContext;
import javax.servlet.AsyncEvent;
import javax.servlet.AsyncListener;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

public class ReverseHttpRequestServlet extends HttpServlet {
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        log("ReverseHttpRequestServlet.doGet");

        process(request, response);
    }

    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        log("ReverseHttpRequestServlet.doPost");

        process(request, response);
    }

    private void process(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String requestId = WaitingContexts.getRequestId(request);
        if (requestId == null) {
            System.out.println("**************> 1");
            response.sendError(HttpServletResponse.SC_PRECONDITION_FAILED, "There is no request id");
            return;
        }

        AsyncContext waitingCtx = WaitingContexts.getInstance().getReadyContext(requestId);
        if (waitingCtx == null) {
            //todo svl need put on wait
            System.out.println("**************> 2");
            response.sendError(HttpServletResponse.SC_PRECONDITION_FAILED, "Not implemented");
            return;
        }

        ReverseHttpUtil.copyRequestToResponse(request, (HttpServletResponse) waitingCtx.getResponse());
        WaitingContexts.getInstance().removeReadyContext(waitingCtx);
        waitingCtx.complete();

        final AsyncContext ctx = request.startAsync();
        WaitingContexts.getInstance().addWaitingResponseContext(ctx);
        ctx.setTimeout(60*60*1000);
        ctx.addListener(new ReverseHttpAsyncListener());
    }

    private void sendHeartbeat(AsyncEvent event) throws IOException {
        log("sending Heartbeat");
        PrintWriter writer = new PrintWriter(new OutputStreamWriter(event.getAsyncContext().getResponse().getOutputStream()));
        writer.println(WaitingContexts.HEARTBEAT_MESSAGE);
        writer.flush();
    }

    private class ReverseHttpAsyncListener implements AsyncListener {
        public void onComplete(AsyncEvent event) throws IOException {
            log("onComplete called");
        }

        public void onTimeout(AsyncEvent event) throws IOException {
            sendHeartbeat(event);
        }

        public void onError(AsyncEvent event) throws IOException {
            log("onError called");
        }

        public void onStartAsync(AsyncEvent event) throws IOException {
            log("onStartAsync called");
        }
    }
}
