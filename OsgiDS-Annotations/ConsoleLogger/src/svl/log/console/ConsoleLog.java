package svl.log.console;

import aQute.bnd.annotation.component.Component;
import svl.log.api.Log;

import java.io.PrintStream;
import java.util.Comparator;

@Component(provide=Log.class)
public class ConsoleLog implements Log {
    public ConsoleLog() {
        System.out.println("ConsoleLog created...");
    }

    public void log(LEVEL level, String message, Throwable t) {
        PrintStream dest = System.out;

        if (level == LEVEL.ERROR)
            dest = System.err;

        dest.println("" + level + "  " + message);
        if (t != null)
            t.printStackTrace(dest);
    }
}
