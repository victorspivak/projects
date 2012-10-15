/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.httpclient;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class ReverseHttpServerRequest implements Runnable {
    private final String url;
    private final String requestId;

    public static void main(String[] args) throws IOException, InterruptedException {
        ExecutorService executorService = Executors.newCachedThreadPool();

        startRequests(executorService, 5);

        executorService.shutdown();
        executorService.awaitTermination(60*24, TimeUnit.MINUTES);
        executorService.shutdownNow();
        System.exit(0);
    }

    private static void startRequests(ExecutorService executorService, int count) {
        for (int i = 0; i < count; i++) {
            executorService.submit(new ReverseHttpServerRequest("http://localhost:8080/reversehttprequest", "Req-" + i));
        }
    }

    public ReverseHttpServerRequest(String url, String requestId) {
        this.url = url;
        this.requestId = requestId;
    }

    public void run() {
        try {
            HttpURLConnection connection = null;
            BufferedReader reader = null;

            try {
                connection = createConnection();
                connection.setDoOutput(true);
                PrintWriter printer = null;
                try {
                    printer = new PrintWriter(new OutputStreamWriter(connection.getOutputStream()));
                    for (int i = 0; i < 10; i++) {
                        printer.println("Request line " + i);
                    }
                    printer.println();
                    printer.flush();
                } catch (IOException e) {
                    printer.close();
                }

                try {
                    reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                    String line;
                    while ((line = reader.readLine()) != null) {
                        System.out.println(line);
                    }
                } catch (IOException e) {
                    e.printStackTrace();
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
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private HttpURLConnection createConnection() throws IOException {
        HttpURLConnection connection = (HttpURLConnection) new URL(url).openConnection();
        connection.addRequestProperty(WaitingContextsConstants.REVERSE_HTTP_REQUEST_ID, requestId);
        return connection;
    }
}