package svl.learn.jackson;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

public class Asset {
    private AssetType type;

    @JsonTypeInfo(
            use = JsonTypeInfo.Id.NAME,
            include = JsonTypeInfo.As.EXTERNAL_PROPERTY,
            property = "type"
    )
    @JsonSubTypes({
            @JsonSubTypes.Type(value = ImageAssetProperties.class, name = "image"),
            @JsonSubTypes.Type(value = DocumentAssetProperties.class, name = "document")
    })
    private AssetProperties properties;

    public String getType() {
        return type.toString();
    }

    @JsonProperty("type")
    public void setType(String type) {
        this.type = AssetType.fromString(type);
    }

    @JsonIgnore
    public void setType(AssetType type) {
        this.type = type;
    }

    public AssetProperties getProperties() {
        return properties;
    }

    public void setProperties(AssetProperties properties) {
        this.properties = properties;
    }
}
