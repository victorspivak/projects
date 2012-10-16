/*
 * Copyright � 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.httpclient;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicHeader;

import java.io.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class ReverseHttpServerRequest implements Runnable {
    private final String url;
    private final String requestId;
    private HttpClient httpclient;

    public static void main(String[] args) throws IOException, InterruptedException {
        ExecutorService executorService = Executors.newCachedThreadPool();

        startRequests(executorService, 1);

        executorService.shutdown();
        executorService.awaitTermination(60*24, TimeUnit.MINUTES);
        executorService.shutdownNow();
        System.exit(0);
    }

    private static void startRequests(ExecutorService executorService, int count) {
        for (int i = 0; i < count; i++) {
            executorService.submit(new ReverseHttpServerRequest("http://localhost:"  + WaitingContextsConstants.PROXY_PORT +
                    "/reversehttprequest", "Req-" + i));
        }
    }

    public ReverseHttpServerRequest(String url, String requestId) {
        this.url = url;
        this.requestId = requestId;
        httpclient = new DefaultHttpClient();
    }

    public void run() {
        try {
            HttpPost httppost = prepareRequest();
            HttpResponse response = httpclient.execute(httppost);
            processResponse(response);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private HttpPost prepareRequest() throws UnsupportedEncodingException {
        StringWriter stringWriter = new StringWriter(1024);
        PrintWriter printer = new PrintWriter(stringWriter);

        for (int i = 0; i < 10; i++) {
            printer.println("Request line " + i);
        }
        printer.println();
        printer.flush();

        HttpPost httppost = new HttpPost(url);
        httppost.addHeader(new BasicHeader(WaitingContextsConstants.REVERSE_HTTP_REQUEST_ID, requestId));
        httppost.setEntity(new StringEntity(stringWriter.getBuffer().toString()));
        return httppost;
    }

    private void processResponse(HttpResponse response) {
        BufferedReader reader = null;

        try {
            HttpEntity entity = response.getEntity();
            if (entity != null) {
                reader = new BufferedReader(new InputStreamReader(entity.getContent()));

                String line;
                while ((line = reader.readLine()) != null) {
                    System.out.println(line);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}