package svl.log.niceconsole;

import aQute.bnd.annotation.component.Component;
import svl.log.api.Log;

import java.io.PrintStream;
import java.text.SimpleDateFormat;
import java.util.Date;

@Component(provide=Log.class)
public class NiceConsoleLog implements Log {
    SimpleDateFormat dateFormat;

    public NiceConsoleLog() {
        System.out.println("NiceConsoleLog created...");
        dateFormat = new SimpleDateFormat();
    }

    public void log(LEVEL level, String message, Throwable t) {
        PrintStream dest = System.out;

        if (level == LEVEL.ERROR)
            dest = System.err;

        dest.println(dateFormat.format(new Date()) + "   " + level + "  " + message);
        if (t != null)
            t.printStackTrace(dest);
    }
}
