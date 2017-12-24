package svl.example.postgres.json;

import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Function;

public class TenantGenerator {
    private final String tenantId;
    private final ThreadLocalRandom random;
    private final Function<ThreadLocalRandom, Map<String, String>> detailsGenerator;

    public TenantGenerator(String tenantId, ThreadLocalRandom random, Function<ThreadLocalRandom, Map<String, String>> detailsGenerator) {
        this.tenantId = tenantId;
        this.random = random;
        this.detailsGenerator = detailsGenerator;
    }

    public String getTenantId() {
        return tenantId;
    }

    public Map<String, String> makeDetails() {
        return detailsGenerator.apply(random);
    }
}