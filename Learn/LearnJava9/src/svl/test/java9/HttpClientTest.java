package svl.test.java9;


import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

public class HttpClientTest {
    public static void main(String[] args) throws InterruptedException, IOException, URISyntaxException {
        HttpClientTest tester = new HttpClientTest();

        tester.simpleGet();
    }

    private void simpleGet() throws URISyntaxException, IOException, InterruptedException {
        HttpClient client = HttpClient.newHttpClient();

        HttpRequest request = HttpRequest.newBuilder()
                .uri(new URI("http://www.google.com"))
                .GET()
                .build();

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.statusCode());
        System.out.println(response.headers().map());
        System.out.println(response.body());
    }
}