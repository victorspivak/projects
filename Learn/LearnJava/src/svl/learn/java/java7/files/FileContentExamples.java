package svl.learn.java.java7.files;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.List;

public class FileContentExamples {
    public static void main(String[] args) {
        try {
            test1();
            System.out.println("============================================");
            test2();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    private static void test1() throws IOException {
        Path path = Paths.get("data/test1.txt");
        Files.write(path, "Hello Java 7\n".getBytes("UTF-8"), StandardOpenOption.CREATE);
        Files.write(path, "Another Hello Java 7\n".getBytes("UTF-8"), StandardOpenOption.APPEND);
        Files.write(path, "Another Hello Java 7\nline 1\nline 2\n".getBytes("UTF-8"), StandardOpenOption.APPEND);

        List<String> lines = Files.readAllLines(path);
        lines.stream().forEach(System.out::println);
    }

    /**
     * Use of Buffered reader
     * @throws IOException
     */
    private static void test2() throws IOException {
        Path path = Paths.get("data/test1.txt");
        Charset charset = Charset.forName("ISO-8859-1");
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
        }
    }
}