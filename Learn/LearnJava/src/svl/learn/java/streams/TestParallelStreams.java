/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.streams;

import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

public class TestParallelStreams {
    public static void main(String[] args) {
        System.out.print("Serial version:");
        timingTest(StreamsTestHelper.makePersonsAsStream());
        int cores = Runtime.getRuntime().availableProcessors();
        System.out.printf("Parallel version (%d cores):", cores);
        timingTest(StreamsTestHelper.makePersonsAsStream().parallel());
        System.out.printf("Serial version (%d elements):", cores);
        timingTest(StreamsTestHelper.makePersonsAsStream().limit(cores));
        System.out.printf("Parallel version (%d elements) (%d cores):", cores, cores);
        timingTest(StreamsTestHelper.makePersonsAsStream().parallel().limit(cores));
    }

    private static void doSlow(Person person){
        try {
            TimeUnit.SECONDS.sleep(1);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
    private static void timingTest(Stream<Person> persons){
        long start = System.nanoTime();
        persons.forEach(TestParallelStreams::doSlow);
        long end = System.nanoTime();
        System.out.printf("Timing: %.3f seconds. \n", deltaSeconds(start, end));
    }

    private static double deltaSeconds(long startTime, long endTime){
        return (endTime - startTime) / 1000000000;
    }
}