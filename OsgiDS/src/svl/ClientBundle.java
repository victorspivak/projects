/*
 * Copyright � 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl;

import org.osgi.service.component.ComponentContext;

import java.util.Comparator;
import java.util.Dictionary;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class ClientBundle {
    private ExecutorService executorService;
    private Comparator comparator;

    public ClientBundle() {
        System.out.println("CLIENT Constructor");
    }

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

    protected void deactivate(ComponentContext context)
    {
        System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> deactivate");
        System.out.println("Goodbye Spring World!!");
        executorService.shutdownNow();
    }

    public void setComparator (Comparator comparator) {
        this.comparator = comparator;
        System.out.println("@@@@@@@@@@@@@@@@@ Comparator @@@@@@@@@@@@@@@@@@@@@@@@");
    }

    public void start(final Object o1, final Object o2) {
        System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> start");
        executorService = Executors.newSingleThreadExecutor();
        executorService.submit(new Runnable() {
            public void run() {
                while (!Thread.interrupted()) {
                    System.out.println("Hello World!! " + comparator.compare(o1, o2));
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