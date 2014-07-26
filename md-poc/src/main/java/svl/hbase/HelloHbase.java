/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.hbase;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;

import org.apache.hadoop.hbase.HBaseConfiguration;

import org.apache.hadoop.hbase.HColumnDescriptor;

import org.apache.hadoop.hbase.HTableDescriptor;

import org.apache.hadoop.hbase.client.HBaseAdmin;

public class HelloHbase {
    public static void main(String[] args) throws IOException
    {
        HBaseConfiguration hc = new HBaseConfiguration(new Configuration());

        HTableDescriptor ht = new HTableDescriptor("User");

        ht.addFamily( new HColumnDescriptor("Id"));

        ht.addFamily( new HColumnDescriptor("Name"));

        System.out.println( "connecting" );

        HBaseAdmin hba = new HBaseAdmin( hc );

        System.out.println( "Creating Table" );

        hba.createTable( ht );

        System.out.println("Done......");
    }
}