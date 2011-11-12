/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.test.performance;

import java.math.BigInteger;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

public class PerformanceBenchmark {

    private static Runnable factorial = new Runnable() {
            public void run() {
                BigInteger factorial = new BigInteger("1");
                int n = 10000;

                for (int j = 1; j < n; j++) {
                    factorial = factorial.multiply(new BigInteger("" + j));
                }

//                    System.out.println("factorial = " + factorial);
            }
        };

    private static Runnable factorial1 = new Runnable() {
            public void run() {
                for (long i = 0; i < 50000000; i++) {
                    long factorial = 1;
                    int n = 40;

                    for (int j = 1; j < n; j++) {
                        factorial = factorial * j;
                    }

                    foo(factorial);
                }
            }
        };

    private static boolean firstTime = true;
    private static AtomicLong callCount = new AtomicLong(0);
    private static void foo(long factorial) {
        if (firstTime) {
            firstTime   = false;
            System.out.println("factorial = " + factorial);
        }

        callCount.incrementAndGet();
    }

    public static void main(String[] args) throws InterruptedException {
        long    start   = System.currentTimeMillis();

        for (int i = 0; i < 5; i++) {
            test();
        }

        long    time    = System.currentTimeMillis() - start;
        System.out.println("Total time = " + time);
        System.out.println("callCount = " + callCount);
    }

    private static void test() throws InterruptedException {
        int threadCount = 12;
        long    start   = System.currentTimeMillis();

        execute(threadCount, factorial1);

        long    time    = System.currentTimeMillis() - start;
        System.out.println("time = " + time);
    }

    private static void execute(int threadCount, Runnable task) throws InterruptedException {
        ExecutorService executor = Executors.newFixedThreadPool(threadCount);

        for (int i = 0; i < threadCount; i++) {
            executor.submit(task);
        }

        executor.shutdown();
        executor.awaitTermination(1, TimeUnit.DAYS);
    }
}