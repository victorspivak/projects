package svl.learn.jackson;

public class DocumentAssetProperties implements AssetProperties {
    private String source;
    private String proxy;

    public String getSource() {
        return source;
    }

    public DocumentAssetProperties setSource(String source) {
        this.source = source;
        return this;
    }

    public String getProxy() {
        return proxy;
    }

    public DocumentAssetProperties setProxy(String proxy) {
        this.proxy = proxy;
        return this;
    }
}
