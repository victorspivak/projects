package svl.learn.java.streams.to_stream;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * FileParser
 *
 * @author victor.spivak
 * @since 212
 */
public class FileParser {
    private final String filename;
    private final BufferedReader reader;
    private StringBuilder logLineBuilder;
    private boolean endOfFile = false;


    public FileParser(String filename) {
        try {
            this.filename = filename;
            this.reader = new BufferedReader(new FileReader(filename));
            this.logLineBuilder = null;
        } catch (FileNotFoundException e) {
            throw new RuntimeException("Could not find " + filename, e);
        }
    }

    public String getLine() {
        String logLine = null;
        String line;

        if (endOfFile) {
            return null;
        }

        try {
            while ((line = reader.readLine()) != null) {

                int index = line.indexOf('`');

                if (index > 0 && index < 7) {
                    if (logLineBuilder != null) {
                        logLine = logLineBuilder.toString();
                    }
                    logLineBuilder = new StringBuilder();
                }

                if (logLineBuilder != null) {
                    logLineBuilder.append(line).append(System.lineSeparator());
                }

                if (logLine != null) {
                    return logLine;
                }
            }
        } catch (IOException e) {
            throw new RuntimeException("Could not read from " + filename, e);
        }
        endOfFile = true;

        return logLineBuilder.toString();
    }

    public void close() {
        try {
            reader.close();
        } catch (IOException e) {
            throw new RuntimeException("Could not close the input stream for " + filename, e);
        }
    }
}
