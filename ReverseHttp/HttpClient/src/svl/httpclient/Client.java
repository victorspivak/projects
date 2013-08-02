/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.httpclient;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class Client implements Runnable {

    private String url;

    public static void main(String[] args) throws IOException, InterruptedException {
        ExecutorService executorService = Executors.newCachedThreadPool();

        startShortRequests(executorService, 200);
        startLongRequests(executorService, 200);
        startShortRequests(executorService, 500);

        executorService.awaitTermination(50, TimeUnit.MINUTES);
        executorService.shutdownNow();
        System.exit(0);
    }

    private static void startLongRequests(ExecutorService executorService, int count) {
        for (int i = 0; i < count; i++) {
            executorService.submit(new Client("http://localhost:8080/helloasync"));
        }
    }

    private static void startShortRequests(ExecutorService executorService, int count) {
        for (int i = 0; i < count; i++) {
            executorService.submit(new Client("http://localhost:8080/hello"));
        }
    }

    public Client(String url) {
        this.url = url;
    }

    public void run() {
        HttpURLConnection connection = null;
        BufferedReader reader = null;

        try {
            System.out.print(".");
            connection = (HttpURLConnection) new URL(url).openConnection();
            reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));

            String line;
            while ((line = reader.readLine()) != null)
                System.out.println(line);
        } catch (IOException e) {
            e.printStackTrace();
        }
        finally {
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