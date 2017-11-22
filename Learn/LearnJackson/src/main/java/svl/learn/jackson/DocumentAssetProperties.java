package svl.learn.jackson;

import java.util.Objects;
import java.util.StringJoiner;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DocumentAssetProperties that = (DocumentAssetProperties) o;

        return Objects.equals(this.proxy, that.proxy) &&
                Objects.equals(this.source, that.source);
    }

    @Override
    public int hashCode() {
        return Objects.hash(proxy, source);
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", this.getClass().getSimpleName() + "[", "]")
                .add("proxy = " + proxy)
                .add("source = " + source)
                .toString();
    }
}
