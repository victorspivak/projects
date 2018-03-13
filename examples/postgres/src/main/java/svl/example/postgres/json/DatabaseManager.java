package svl.example.postgres.json;

import java.sql.*;

public class DatabaseManager {
    private static final String DB_URL = "jdbc:postgresql://localhost/victor";
    private static final String DB_USER = "victor";
    private static final String DB_PASSWORD = "admin";

    public static final String TABLE_NAME = "my_invoices";
    public static final String JASON_COLUMN_NAME = "details";

    public Connection getConnection() throws SQLException {
        return DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
    }

    public void makeTable() throws SQLException {
        try (Connection con = getConnection();
             Statement st = con.createStatement()) {

            st.executeUpdate(String.format("DROP TABLE IF EXISTS %s;", TABLE_NAME));
            st.executeUpdate(String.format("DROP SEQUENCE IF EXISTS %s_seq;", TABLE_NAME));

            st.executeUpdate(String.format("CREATE SEQUENCE %s_seq;", TABLE_NAME));
            //for seq number id use: ID serial NOT NULL PRIMARY KEY
            st.executeUpdate(String.format("CREATE TABLE %s (invoice_id char(16) NOT NULL PRIMARY KEY DEFAULT 'INV' || to_char(nextval('%s_seq'), 'FM000000000000'), tenantId char(16) NOT NULL, %s jsonb NOT NULL);", TABLE_NAME, TABLE_NAME, JASON_COLUMN_NAME));
            st.executeUpdate(String.format("CREATE INDEX ind_%s_%s ON %s USING gin (%s);", TABLE_NAME, JASON_COLUMN_NAME, TABLE_NAME, JASON_COLUMN_NAME));
        }
    }

    public String getInsertStatement() {
        return String.format("INSERT INTO %s(tenantId, details) VALUES(?, to_json(?::json))", TABLE_NAME);
    }
}