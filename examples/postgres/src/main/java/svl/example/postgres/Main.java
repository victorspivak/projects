package svl.example.postgres;

import com.google.common.base.Stopwatch;
import svl.example.postgres.json.DatabaseManager;
import svl.example.postgres.json.RecordGenerator;

import java.io.IOException;
import java.sql.*;
import java.util.concurrent.TimeUnit;

public class Main {
    private DatabaseManager databaseManager;
    private RecordGenerator generator;

    public static void main(String[] args) throws SQLException, IOException {
        Main main = new Main(new DatabaseManager());

        if (false)
            main.populate(true, 1_000_000);

        main.query();
    }

    private Main(DatabaseManager databaseManager) throws IOException {
        this.databaseManager = databaseManager;
        generator = new RecordGenerator();
    }

    private void query() throws SQLException {
        Stopwatch stopwatch = Stopwatch.createStarted();

        String sql = String.format("select tenantId, details->'vendor' from my_invoices where details @> '{\"approved\": \"false\"}'",
                DatabaseManager.TABLE_NAME);

        try (Connection con = databaseManager.getConnection();
             Statement st = con.createStatement();
             ResultSet rs = st.executeQuery(sql)) {

            int count = 0;
            while (rs.next()) {
                if (count <10)
                    System.out.println(String.format("|%s|%s|", rs.getString(1), rs.getString(2)));
                count += 1;
            }

            System.out.println(String.format("Total count: %d  timing: %d", count, stopwatch.elapsed(TimeUnit.MILLISECONDS)));
        }

        stopwatch.stop();
    }

    private void populate(boolean flushTable, int count) throws SQLException {
        if (flushTable)
            databaseManager.makeTable();

        Stopwatch stopwatch = Stopwatch.createStarted();

        try (Connection con = databaseManager.getConnection();
             PreparedStatement st = con.prepareStatement(databaseManager.getInsertStatement())) {

            for (int i = 0; i < count; i++) {
                RecordGenerator.Record record = generator.makeRecord();

                st.setString(1, record.getTenantId());
                st.setString(2, record.getDetails());
                st.executeUpdate();

                if ((i % 1000) == 0 && i != 0)
                    System.out.println(String.format("progress: %d  timing %d", i, stopwatch.elapsed(TimeUnit.MILLISECONDS)));
            }
        }

        stopwatch.stop();
        System.out.println("Populate timing: " + stopwatch.elapsed(TimeUnit.MILLISECONDS));
    }
}