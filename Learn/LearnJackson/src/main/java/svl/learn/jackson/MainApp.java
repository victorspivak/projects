package svl.learn.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;

public class MainApp {
    public static void main(String[] args) throws IOException {
        String json = "{ \"type\": \"document\", \"properties\": { \"source\": \"foo\", \"proxy\": \"bar\" } }";

        ObjectMapper mapper = new ObjectMapper();
        Asset asset = mapper.readValue(json, Asset.class);

        String json1 = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(asset);

        System.out.println("json1 = " + json1);

        Asset asset1 = mapper.readValue(json1, Asset.class);
        System.out.println("asset1 = " + asset1);

    }
}
