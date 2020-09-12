/*
 * Copyright 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.servlet.reversehttp;

import com.google.common.collect.Maps;

import javax.servlet.AsyncContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class WaitingContexts {
    public final static String REVERSE_HTTP_REQUEST_ID = "ReverseHttpRequestId";
    public final static String HEARTBEAT_MESSAGE = "Heartbeat message";
    public final static long HEARTBEAT_TIMEOUT = 5*1000;

    private static final WaitingContexts instance = new WaitingContexts();
    private Map<String, AsyncContext> readyContexts;
    private Map<String, AsyncContext> watingContexts;
    private Map<String, AsyncContext> waitingResponseContexts;

    private WaitingContexts() {
        readyContexts = Maps.newHashMap();
        watingContexts = Maps.newHashMap();
        waitingResponseContexts = Maps.newHashMap();
    }

    public static WaitingContexts getInstance() {
        return instance;
    }

    public void addReadyContext(AsyncContext context) {
        readyContexts.put(getRequestId(context), context);
    }

    public void removeReadyContext(AsyncContext context) {
        readyContexts.remove(getRequestId(context));
    }

    public AsyncContext getReadyContext(String requestId) {
        return readyContexts.get(requestId);
    }

    public void addWaitingContext(AsyncContext context) {
        watingContexts.put(getRequestId(context), context);
    }

    public void removeWaitingContext(AsyncContext context) {
        watingContexts.remove(getRequestId(context));
    }

    public AsyncContext getWaitingContext(String requestId) {
        return watingContexts.get(requestId);
    }

    public void addWaitingResponseContext(AsyncContext context) {
        waitingResponseContexts.put(getRequestId(context), context);
    }

    public void removeWaitingResponseContext(AsyncContext context) {
        waitingResponseContexts.remove(getRequestId(context));
    }

    public AsyncContext getWaitingResponseContext(String requestId) {
        return waitingResponseContexts.get(requestId);
    }

    public static String getRequestId(ServletRequest request) {
        return ((HttpServletRequest)request).getHeader(WaitingContexts.REVERSE_HTTP_REQUEST_ID);
    }

    private static String getRequestId(AsyncContext context) {
        return getRequestId(context.getRequest());
    }
}