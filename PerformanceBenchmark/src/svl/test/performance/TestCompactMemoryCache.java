/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.test.performance;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.zip.DeflaterOutputStream;

public class TestCompactMemoryCache {
    public static void main(String[] args) throws IOException {
        Map<String, Integer> map = new HashMap<String, Integer>();

        for (int i = 0; i < 100; i++) {
            map.put("" + i, i);
        }

        ByteArrayOutputStream buffer1 = new ByteArrayOutputStream();
        ObjectOutputStream out1 = new ObjectOutputStream(buffer1);

        out1.writeObject(map);

        out1.flush();

        System.out.println("buffer1 = " + buffer1.size());

        ByteArrayOutputStream buffer2 = new ByteArrayOutputStream();
        DeflaterOutputStream zip = new DeflaterOutputStream(buffer2);
        ObjectOutputStream out2 = new ObjectOutputStream(zip);

        out2.writeObject(map);

        out2.flush();
        zip.finish();
        zip.flush();
        System.out.println("buffer2 = " + buffer2.size());
    }
}