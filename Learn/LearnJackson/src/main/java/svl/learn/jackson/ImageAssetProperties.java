package svl.learn.jackson;

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
}
