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

public class ClientBundle {
    private ExecutorService executorService;
    private Comparator comparator;
    private Log logger;

    public ClientBundle() {
        System.out.println("CLIENT Constructor");
    }

    @SuppressWarnings({"unused"})
    protected void activate(ComponentContext context)
    {
        System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> activate");

        Dictionary properties = context.getProperties();
        boolean compareMode = true;

        if (properties != null) {
            compareMode = (Boolean) properties.get("mode");
        }

        if (compareMode)
            start("1", "1");
        else
            start("1", "2");
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
        System.out.println("@@@@@@@@@@@@@@@@@ Comparator @@@@@@@@@@@@@@@@@@@@@@@@");
    }

    @SuppressWarnings({"unused"})
    public void setLog(Log logger) {
        this.logger = logger;
    }

    @SuppressWarnings({"unchecked"})
    public void start(final Object o1, final Object o2) {
        System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> start");
        executorService = Executors.newSingleThreadExecutor();
        executorService.submit(new Runnable() {
            public void run() {
                while (!Thread.interrupted()) {
                    logger.log(Log.LEVEL.DEBUG, "Hello World!! " + comparator.compare(o1, o2), null);
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {
                        System.out.println("************************************************************************");
                        Thread.currentThread().interrupt();
                    }
                }
            }
        });
    }
}