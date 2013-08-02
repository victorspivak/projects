/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.servlet.reversehttp;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.util.Enumeration;

public class ReverseHttpUtil {
    public static void copyRequestToResponse(HttpServletRequest request, HttpServletResponse response) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(request.getInputStream()));
        PrintWriter writer = new PrintWriter(new OutputStreamWriter(response.getOutputStream()));

        writer.println(request.getRequestURI());
        writer.println();

        Enumeration<String> headerNames = request.getHeaderNames();
        while (headerNames.hasMoreElements()) {
            String name = headerNames.nextElement();
            Enumeration<String> headers = request.getHeaders(name);

            while (headers.hasMoreElements()) {
                String header = headers.nextElement();
                writer.println(String.format("%s : %s", name, header));
            }

            writer.println();
        }

        String line;
        while ((line = reader.readLine()) != null)
            writer.println(line);

        writer.flush();
    }
}