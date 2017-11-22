package svl.learn.jackson;

public enum AssetType {
    IMAGE("image"),
    DOCUMENT("document");

    private String string;

    AssetType(String string) {
        this.string = string;
    }

    public static AssetType fromString(String string) {
        // Enum.valueOf() method is case sensitive; this method is not.
        if (string != null)
            for (AssetType assetType : AssetType.values())
                if (string.equalsIgnoreCase(assetType.toString()))
                    return assetType;
        throw new IllegalArgumentException(String.format("%s is not a valid AssetType.", string));
    }

    public String toString() {
        return this.string;
    }
}
