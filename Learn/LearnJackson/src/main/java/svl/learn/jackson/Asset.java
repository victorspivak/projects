package svl.learn.jackson;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

import java.util.Objects;
import java.util.StringJoiner;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Asset that = (Asset) o;

        return Objects.equals(this.properties, that.properties) &&
                Objects.equals(this.type, that.type);
    }

    @Override
    public int hashCode() {
        return Objects.hash(properties, type);
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", this.getClass().getSimpleName() + "[", "]")
                .add("properties = " + properties)
                .add("type = " + type)
                .toString();
    }
}
