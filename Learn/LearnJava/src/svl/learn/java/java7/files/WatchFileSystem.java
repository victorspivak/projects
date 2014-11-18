package svl.learn.java.java7.files;

import java.io.IOException;
import java.nio.file.*;

import static java.nio.file.StandardWatchEventKinds.*;

public class WatchFileSystem {

    public static void main(String[] args) {
        try (WatchService watcher = FileSystems.getDefault().newWatchService()){
            Path dir = Paths.get(".");
            dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);

            while (true) {
                // wait for a key to be available
                WatchKey key = watcher.take();

                processEvents(key);

                // IMPORTANT: The key must be reset after processed
                boolean valid = key.reset();
                if (!valid) {
                    break;
                }
            }
        } catch(IOException e){
            e.printStackTrace();
        } catch (InterruptedException ignored) {
            System.out.println("Interrupted");
        }
    }

    private static void processEvents(WatchKey key) {
        for (WatchEvent<?> event : key.pollEvents()) {
            // get event type
            WatchEvent.Kind<?> kind = event.kind();

            // get file name
            @SuppressWarnings("unchecked")
            WatchEvent<Path> ev = (WatchEvent<Path>) event;
            Path fileName = ev.context();

            System.out.printf("Event %s for %s file\n", kind.name(), fileName);
        }
    }
}