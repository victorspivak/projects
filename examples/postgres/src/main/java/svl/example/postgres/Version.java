package svl.example.postgres;

import svl.example.postgres.json.DatabaseManager;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Version {

    public static void main(String[] args) {

        DatabaseManager databaseManager = new DatabaseManager();

        try (Connection con = databaseManager.getConnection();
             Statement st = con.createStatement();
             ResultSet rs = st.executeQuery("SELECT VERSION()")) {

            if (rs.next()) {
                System.out.println(rs.getString(1));
            }
        } catch (SQLException ex) {
            Logger lgr = Logger.getLogger(Version.class.getName());
            lgr.log(Level.WARNING, ex.getMessage(), ex);
        }
    }
}
