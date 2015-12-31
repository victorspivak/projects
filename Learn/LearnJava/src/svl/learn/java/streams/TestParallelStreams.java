/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.streams;

import svl.learn.java.java8.trymonad.Try;

import java.util.concurrent.*;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class TestParallelStreams {
    public static void main(String[] args) throws ExecutionException, InterruptedException {
        System.out.print("Serial version:");
        timingTest(StreamsTestHelper.makePersonsAsStream());
        int cores = Runtime.getRuntime().availableProcessors();
        System.out.printf("Parallel version (%d cores):", cores);
        timingTest(StreamsTestHelper.makePersonsAsStream().parallel());
        System.out.printf("Serial version (%d elements):", cores);
        timingTest(StreamsTestHelper.makePersonsAsStream().limit(cores));
        System.out.printf("Parallel version (%d elements) (%d cores):", cores, cores);
        timingTest(StreamsTestHelper.makePersonsAsStream().parallel().limit(cores));

        System.out.println("20 elements seq");
        timingTest(IntStream.range(1, 20).boxed());
        System.out.println("20 elements par");
        timingTest(IntStream.range(1, 20).boxed().parallel());

        System.out.println("20 elements par custom thread count");
        timingTest(() -> {
            ForkJoinPool forkJoinPool = new ForkJoinPool(20);
            ForkJoinTask<?> task = forkJoinPool.submit(() -> IntStream.range(1, 20).boxed().parallel().forEach(TestParallelStreams::doSlow));
            Try.tryOf((Callable<Object>) task::get);
        });
        //Also it is possible to control number of threads using the following property
        //System.setProperty("java.util.concurrent.ForkJoinPool.common.parallelism", "20");
    }

    private static<T> void doSlow(T obj){
        try {
            TimeUnit.SECONDS.sleep(1);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private static<T> void timingTest(Runnable runnable){
        long start = System.nanoTime();
        runnable.run();
        long end = System.nanoTime();
        System.out.printf("Timing: %.3f seconds. \n", deltaSeconds(start, end));
    }

    private static<T> void timingTest(Stream<T> stream){
        timingTest(() -> stream.forEach(TestParallelStreams::doSlow));
    }

    private static double deltaSeconds(long startTime, long endTime){
        return (endTime - startTime) / 1000000000;
    }
}