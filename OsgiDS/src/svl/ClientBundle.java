/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl;

import org.osgi.service.component.ComponentContext;

import java.util.Comparator;
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
        start();
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

    public void start() {
        System.out.println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> start");
        executorService = Executors.newSingleThreadExecutor();
        executorService.submit(new Runnable() {
            public void run() {
                while (!Thread.interrupted()) {
                    System.out.println("Hello World!! " + comparator.compare("1", "1"));
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