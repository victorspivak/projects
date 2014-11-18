package svl.learn.java.java7.files;

import java.io.IOException;
import java.nio.file.*;
import java.util.Map;
import java.util.stream.StreamSupport;

public class PathExamples {
    public static void main(String[] args) throws IOException {
        Path path = Paths.get(".");
        System.out.println("path = " + path);

        Path normalizedPath = path.normalize();
        System.out.println("normalizedPath = " + normalizedPath);
        Path absPath = path.toAbsolutePath();
        System.out.println("absPath = " + absPath);
        Path subpath = absPath.subpath(2, 4);
        System.out.println("subpath = " + subpath);
        System.out.println("absPath.getNameCount() = " + absPath.getNameCount());

        System.out.println();
        System.out.println("=====================================================================");
        FileAttributesExample();

        System.out.println();
        System.out.println("=====================================================================");
        FileSystem fs = path.getFileSystem();
        System.out.println("fs = " + fs);
        Iterable<FileStore> fileStores = fs.getFileStores();
        StreamSupport.stream(fileStores.spliterator(), false).forEach(store -> System.out.println("store = " + store));
    }

    private static void FileAttributesExample() throws IOException {
        Path path = Paths.get("a.txt");
        if (Files.exists(path))
            Files.delete(path);
        Files.createFile(path);
        System.out.println("path = " + path);
        String contentType = Files.probeContentType(path);
        System.out.println("contentType = " + contentType);
        Map<String, Object> attributes = Files.readAttributes(path, "*");
        System.out.println("attributes = " + attributes);
        Files.delete(path);
    }
}