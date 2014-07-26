/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.hbase;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.hbase.HBaseConfiguration;
import org.apache.hadoop.hbase.HColumnDescriptor;
import org.apache.hadoop.hbase.HTableDescriptor;
import org.apache.hadoop.hbase.client.HBaseAdmin;

import java.io.IOException;

@SuppressWarnings({"UseOfSystemOutOrSystemErr", "UtilityClass", "Annotation", "BusyWait"})
public class HelloHbase {
    private HelloHbase() {
    }

    public static void main(String[] args) throws IOException, InterruptedException {
        HBaseConfiguration hc = new HBaseConfiguration(new Configuration());

        String tableName = "User";
        HTableDescriptor ht = new HTableDescriptor(tableName);

        ht.addFamily(new HColumnDescriptor("Id"));

        ht.addFamily(new HColumnDescriptor("Name"));

        System.out.println("connecting");

        HBaseAdmin hba = new HBaseAdmin(hc);


        for (int i = 0; i <100; i++) {
            if (!hba.tableExists(tableName)){
                System.out.println("Creating Table");
                hba.createTable(ht);
            }
            else
                System.out.println("The table exists");
            Thread.sleep(1000);
        }

        System.out.println("Done......");
    }
}