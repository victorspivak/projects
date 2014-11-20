package svl.learn.java.java7.files;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

public class FileSystemWalker {
    public static void main(String[] args) throws IOException {
        Path path = Paths.get(".");
        ListFiles listFiles = new ListFiles();
        Files.walkFileTree(path, listFiles);
    }

    static private class ListFiles extends SimpleFileVisitor<Path> {
        private final int indentionAmount = 3;
        private int indentionLevel;

        public ListFiles() {
            indentionLevel = 0;
        }

        private void indent() {
            for(int i=0 ; i<indentionLevel; i++) {
                System.out.print(' ');
            }
        }

        @Override
        public FileVisitResult visitFile(Path file, BasicFileAttributes attributes) {
            indent();
            System.out.println(file.getFileName());
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult postVisitDirectory(Path directory, IOException e) throws IOException {
            indentionLevel -= indentionAmount;
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult preVisitDirectory(Path directory, BasicFileAttributes attributes) throws IOException {
            if (directory.getNameCount() > 1){
                String dirName = directory.getName(directory.getNameCount() - 1).toString();
                if (dirName.startsWith(".") || dirName.equals("out"))
                    return FileVisitResult.SKIP_SUBTREE;
            }
            indent();
            System.out.println(directory.getFileName());
            indentionLevel += indentionAmount;
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
            System.out.println("A file traversal error ocurred");
            return super.visitFileFailed(file, exc);
        }
    }
}