package svl.learn.jackson;

import java.util.Objects;
import java.util.StringJoiner;

public class ImageAssetProperties implements AssetProperties {
    private String source;
    private String proxy;
    private Integer height;
    private Integer width;

    public String getSource() {
        return source;
    }

    public ImageAssetProperties setSource(String source) {
        this.source = source;
        return this;
    }

    public String getProxy() {
        return proxy;
    }

    public ImageAssetProperties setProxy(String proxy) {
        this.proxy = proxy;
        return this;
    }

    public Integer getHeight() {
        return height;
    }

    public ImageAssetProperties setHeight(Integer height) {
        this.height = height;
        return this;
    }

    public Integer getWidth() {
        return width;
    }

    public ImageAssetProperties setWidth(Integer width) {
        this.width = width;
        return this;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ImageAssetProperties that = (ImageAssetProperties) o;

        return Objects.equals(this.height, that.height) &&
                Objects.equals(this.proxy, that.proxy) &&
                Objects.equals(this.source, that.source) &&
                Objects.equals(this.width, that.width);
    }

    @Override
    public int hashCode() {
        return Objects.hash(height, proxy, source, width);
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", this.getClass().getSimpleName() + "[", "]")
                .add("height = " + height)
                .add("proxy = " + proxy)
                .add("source = " + source)
                .add("width = " + width)
                .toString();
    }
}
