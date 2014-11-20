package svl.learn.java.java7.files;

import java.io.IOException;
import java.nio.file.*;

public class DirectoryStreamExample {
    public static void main(String[] args) {
        Path directory = Paths.get("./src/svl/learn/java/java7");
        try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(directory, "C*.java")) {
            for (Path file : directoryStream)
                System.out.println(file.getFileName());
        }
        catch (IOException | DirectoryIteratorException ex) {
            ex.printStackTrace();
        }
    }
}