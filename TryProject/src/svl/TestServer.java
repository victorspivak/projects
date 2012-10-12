/*
 * Copyright, 1999- 2012, SALESFORCE.com 
 * All Rights Reserved
 * Company Confidential
 */
package svl;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * TestServer
 *
 * @author vspivak
 * @since 182
 */
public class TestServer {
    public static void main(String[] args) throws IOException {
        ServerSocket socket = new ServerSocket(8888);

        while (true) {
            final Socket client = socket.accept();

            new Thread(new Runnable() {
                public void run() {
                    try {
                        BufferedReader reader = new BufferedReader(new InputStreamReader(client.getInputStream()));
                        PrintWriter printer = new PrintWriter(new OutputStreamWriter(client.getOutputStream()));

                        String line = reader.readLine();
                        line = line == null ? "" : line.trim();
                        long pause = line.isEmpty() ? 10000 : Long.parseLong(line);

                        int counter = 0;

                        while(true) {
                            try {
                                printer.println(counter++);
                                printer.flush();
                                Thread.sleep(pause);
                            } catch (InterruptedException e) {
                                break;
                            }
                        }
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }).start();
        }
    }
}
