/*
 * Copyright � 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl;

import org.osgi.service.component.ComponentContext;
import svl.log.api.Log;

import java.util.Comparator;
import java.util.Dictionary;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicReference;

public class ClientBundle {
    private ExecutorService executorService;
    private Comparator comparator;
    private AtomicReference<Log> loggerRef;

    public ClientBundle() {
        System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> CLIENT Constructor");
        loggerRef = new AtomicReference<Log>();
    }

    @SuppressWarnings({"unused"})
    protected void activate(ComponentContext context)
    {
        System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> activate");

        Dictionary properties = context.getProperties();
        boolean compareMode = Boolean.parseBoolean((String) properties.get("mode"));
        long timeout = Long.parseLong((String) properties.get("timeout"));

        if (compareMode)
            start("1", "1", timeout);
        else
            start("1", "2", timeout);
    }

    @SuppressWarnings({"unused"})
    protected void deactivate(ComponentContext context)
    {
        System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> deactivate");
        System.out.println("Goodbye Spring World!!");
        executorService.shutdownNow();
    }

    @SuppressWarnings({"unused"})
    public void setComparator (Comparator comparator) {
        this.comparator = comparator;
    }

    @SuppressWarnings({"unused"})
    public void setLog(Log logger) {
        loggerRef.set(logger);
        System.out.println("set logger = " + logger);
    }

    @SuppressWarnings({"unused"})
    public void unsetLog(Log logger) {
        System.out.println("unset logger = " + logger);
        loggerRef.compareAndSet(logger, null);
    }

    @SuppressWarnings({"unchecked"})
    public void start(final Object o1, final Object o2, final long timeout) {
        executorService = Executors.newSingleThreadExecutor();
        executorService.submit(new Runnable() {
            @SuppressWarnings({"NullableProblems"})
            public void run() {
                while (!Thread.interrupted()) {
                    Log logger = loggerRef.get();
                    if (logger != null)
                        logger.log(Log.LEVEL.DEBUG, "Hello World!! " + comparator.compare(o1, o2), null);
                    else
                        System.out.println("*** No Logger ***");
                    try {
                        Thread.sleep(timeout);
                    } catch (InterruptedException e) {
                        System.out.println("************************************************************************");
                        Thread.currentThread().interrupt();
                    }
                }
            }
        });
    }
}