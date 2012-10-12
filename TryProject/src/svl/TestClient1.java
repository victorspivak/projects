/*
 * Copyright, 1999- 2012, SALESFORCE.com 
 * All Rights Reserved
 * Company Confidential
 */
package svl;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 * TestClient1
 *
 * @author vspivak
 * @since 182
 */
public class TestClient1 {
    public static void main(String[] args) throws IOException {
        Socket socket = new Socket("localhost", 8888);

        BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        PrintWriter printer = new PrintWriter(new OutputStreamWriter(socket.getOutputStream()));

        printer.println("3000");
        printer.flush();

        while (true) {
            System.out.println(reader.readLine());
        }
    }
}
