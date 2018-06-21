package svl.learn.java.streams.to_stream;

import java.io.IOException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * ReaderToIterator
 *
 * @author victor.spivak
 * @since 212
 */
public class ReaderToIterator implements Iterator<String> {
    private FileParser parser;
    private String current;

    public static Stream<String> logToStream(String filename) throws IOException {
        Iterable<String> iterable = () -> new ReaderToIterator(filename);
        return StreamSupport.stream(iterable.spliterator(), false);
    }

    ReaderToIterator(String filename) {
        this.parser = new FileParser(filename);
        this.current = parser.getLine();
    }

    @Override
    public boolean hasNext() {
        return current != null;
    }

    @Override
    public String next() {
        if (current == null) {
            throw new NoSuchElementException();
        }

        String result = current;
        current = parser.getLine();
        if (current == null) {
            parser.close();
        }

        return result;
    }
}