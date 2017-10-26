package svl.learn.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.fail;

public class PolymorphicDeserializationTests {

    @Test
    public void deserializeJsonSucceeds() {
        Asset asset = deserializeJson("{ \"type\": \"document\", \"properties\": { \"source\": \"foo\", \"proxy\": \"bar\" } }");
        Assert.assertTrue(asset.getProperties() instanceof DocumentAssetProperties);
    }

    public Asset deserializeJson(String json) {
        ObjectMapper mapper = new ObjectMapper();
        try {
            return mapper.readValue(json, Asset.class);
        }
        catch(IOException e) {
            fail("Could not deserialize JSON." + e);
        }
        return null;
    }
}
