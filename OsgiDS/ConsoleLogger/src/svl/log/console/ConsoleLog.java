/*
 * Copyright, 1999- 2011, SALESFORCE.com 
 * All Rights Reserved
 * Company Confidential
 */
package svl.log.console;

import svl.log.api.Log;

import java.io.PrintStream;

/**
 * ConsoleLog
 *
 * @author vspivak
 * @since 174
 */
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
