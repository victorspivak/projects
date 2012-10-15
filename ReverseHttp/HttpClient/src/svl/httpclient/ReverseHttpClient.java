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

public class ReverseHttpClient implements Runnable {
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
            executorService.submit(new ReverseHttpClient("http://localhost:8080/reversehttp", "Req-" + i));
        }
    }

    public ReverseHttpClient(String url, String requestId) {
        this.url = url;
        this.requestId = requestId;
    }

    public void run() {
        HttpURLConnection connection = null;
        BufferedReader reader = null;

        try {
            connection = createConnection();

            while (true) {
                reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));

                String line = readHeartbeats(reader);
                connection = processRequest(connection, line, reader);
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

    private HttpURLConnection processRequest(HttpURLConnection connection, String line, BufferedReader reader) throws IOException {
        while ((line = reader.readLine()) != null) {
            System.out.println(line);
        }

        reader.close();
        connection.disconnect();
        connection = createConnection();
        connection.setDoOutput(true);

        PrintWriter printer = null;
        try {
            printer = new PrintWriter(new OutputStreamWriter(connection.getOutputStream()));
            for (int i = 0; i < 10; i++) {
                printer.println("Response line " + i);
            }
            printer.println();
            printer.flush();
        } catch (IOException e) {
            printer.close();
        }

        return connection;
    }

    private String readHeartbeats(BufferedReader reader) throws IOException {
        String line;
        while ((line = reader.readLine()) != null) {
            if (line.equals(WaitingContextsConstants.HEARTBEAT_MESSAGE))
                break;
            System.out.println(line);
        }
        return line;
    }

    private HttpURLConnection createConnection() throws IOException {
        HttpURLConnection connection = (HttpURLConnection) new URL(url).openConnection();
        connection.addRequestProperty(WaitingContextsConstants.REVERSE_HTTP_REQUEST_ID, requestId);
        return connection;
    }
}