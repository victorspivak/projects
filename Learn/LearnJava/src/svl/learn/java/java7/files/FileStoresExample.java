package svl.learn.java.java7.files;

import svl.learn.java.java8.trymonad.Try;

import java.nio.file.FileStore;
import java.nio.file.FileSystems;
import java.util.stream.StreamSupport;

public class FileStoresExample {
    public static void main(String[] args) throws Exception {
        Iterable<FileStore> fileStores = FileSystems.getDefault().getFileStores();
        StreamSupport.stream(fileStores.spliterator(), false).forEach(store -> {
            System.out.printf("%15s (%20s) -> %s\n", store.name(), store.type(), Try.tryOf(() -> store.getTotalSpace()));
        });
    }
}