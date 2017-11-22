package svl.learn.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

import java.io.IOException;

public class MainApp {
    public static void main(String[] args) throws IOException {
        String json = "{ \"type\": \"document\", \"properties\": { \"source\": \"foo\", \"proxy\": \"bar\" } }";

        ObjectMapper jsonMapper = new ObjectMapper();
        Asset asset = jsonMapper.readValue(json, Asset.class);

        String json1 = jsonMapper.writerWithDefaultPrettyPrinter().writeValueAsString(asset);
        System.out.println("json1 = " + json1);

        Asset asset1 = jsonMapper.readValue(json1, Asset.class);
        System.out.println("asset1 = " + asset1);

        ObjectMapper yamlMapper = new ObjectMapper(new YAMLFactory());
        String yml = yamlMapper.writerWithDefaultPrettyPrinter().writeValueAsString(asset);

        System.out.println("yml = " + yml);

        Asset asset2 = yamlMapper.readValue(json1, Asset.class);
        System.out.println("asset2 = " + asset2);

    }
}
