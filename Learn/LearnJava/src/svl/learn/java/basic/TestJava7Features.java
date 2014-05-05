/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.basic;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InvalidClassException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class TestJava7Features {
    public static void main(String[] args) {

        //Diamond Operator
        List<String> list = new ArrayList<>(11);

        //Using strings in switch statements
        String command = "foo";
        switch (command){
            case "boo":
                System.out.println("boo");
                break;
            case "foo":
                System.out.println("!!!");
                break;
            default:
                System.out.println("Oops");

        }

        //Automatic resource management and multiple exceptions in the catch
        MyFileOutputStream s = null;
        try(MyFileOutputStream out = new MyFileOutputStream("tmpFileToBeDeleted.tmp")){
            s = out;
        }
        catch (IllegalArgumentException|IOException e){
            System.out.println("e = " + e);
        }

        System.out.println("s.isClosed() = " + s.isClosed());

        //Numeric literals with underscores
        int thousand =  1_000;
        int million  =  1_000_000;

        System.out.println("million = " + million);

        //New file system API (NIO 2.0)
        pathInfo();
    }

    public static void pathInfo() {

        Path path = Paths.get("/tmp");

        System.out.println("Number of Nodes:" + path.getNameCount());

        System.out.println("File Name:" + path.getFileName());

        System.out.println("File Root:" + path.getRoot());

        System.out.println("File Parent:" + path.getParent());

    }

    static private class MyFileOutputStream extends FileOutputStream{
        private boolean isClosed = false;
        public MyFileOutputStream(String name) throws FileNotFoundException {
            super(name);
        }

        @Override
        public void close() throws IOException {
            super.close();
            isClosed = true;
        }

        public boolean isClosed() {
            return isClosed;
        }
    }
}