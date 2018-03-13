package svl.example.postgres.json;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.joining;

public class RecordGenerator {
    public static class Record {
        private final String tenantId;
        private final String details;

        public Record(String tenantId, String details) {
            this.tenantId = tenantId;
            this.details = details;
        }

        public String getTenantId() {
            return tenantId;
        }

        public String getDetails() {
            return details;
        }

        @Override
        public String toString() {
            return new StringJoiner(", ", this.getClass().getSimpleName() + "[", "]")
                    .add("details = " + details)
                    .add("tenantId = " + tenantId)
                    .toString();
        }
    }
    private final ThreadLocalRandom random;
    private final ObjectMapper jsonMapper;
    private final List<String> companies;
    private final List<String> countries;
    private final List<String> states;
    private final List<String> words;
    private final List<TenantGenerator> tenants;

    public RecordGenerator() throws IOException {
        jsonMapper = new ObjectMapper();

        companies = loadList("companies.txt");
        countries = loadList("countries.txt");
        states = loadList("states.txt");
        words = loadList("words.txt");

        random = ThreadLocalRandom.current();

        tenants = makeTenantGenerators();
    }

    public Record makeRecord() {
        TenantGenerator generator = tenants.get(random.nextInt(tenants.size()));

        return new Record(generator.getTenantId(), generateJson(generator.makeDetails()));
    }

    private List<TenantGenerator> makeTenantGenerators() {
        return List.of(
                new TenantGenerator("tenant-1", random,
                        r -> Map.of("vendor", randomItem(companies),
                                "amount", String.valueOf(r.nextInt(1000)),
                                "price", String.valueOf(1.0 * r.nextInt(10000) / 100),
                                "product", randomPhrase(2, 5))),
                new TenantGenerator("tenant-2", random,
                        r -> Map.of("vendor", randomItem(companies),
                                "amount", String.valueOf(r.nextInt(1000)),
                                "price", String.valueOf(1.0 * r.nextInt(10000) / 100),
                                "tax", String.valueOf(1.0 * r.nextInt(900) / 100),
                                "country", randomItem(countries))),
                new TenantGenerator("tenant-3", random,
                        r -> Map.of("vendor", randomItem(companies),
                                "amount", String.valueOf(r.nextInt(1000)),
                                "price", String.valueOf(1.0 * r.nextInt(10000) / 100),
                                "state", randomItem(states))),
                new TenantGenerator("tenant-4", random,
                        r -> Map.of("vendor", randomItem(companies),
                                "amount", String.valueOf(r.nextInt(100)),
                                "price", String.valueOf(1.0 * r.nextInt(100000) / 100),
                                "approved", String.valueOf(r.nextBoolean())))
            );
    }

    private String generateJson(Map<String, String> values) {
        try {
            return jsonMapper.writerWithDefaultPrettyPrinter().writeValueAsString(values);
        } catch (JsonProcessingException e) {
            throw new HandledException(e);
        }
    }

    private String randomPhrase(int minLength, int maxLength) {
        return IntStream.iterate(0, i -> i + 1)
                .mapToObj(i -> words.get(random.nextInt(words.size())))
                .limit(random.nextInt(minLength, maxLength))
                .collect(joining(" "));
    }

    private String randomItem(List<String> list) {
        return list.get(random.nextInt(list.size()));
    }

    private List<String> loadList(String filename) throws IOException {
        return Files.lines(Paths.get("data", filename)).collect(Collectors.toList());
    }
}