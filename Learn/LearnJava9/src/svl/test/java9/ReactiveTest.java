package svl.test.java9;

import java.util.concurrent.SubmissionPublisher;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import static java.util.concurrent.Flow.Subscription;
import static java.util.concurrent.Flow.Subscriber;
import static java.util.concurrent.Flow.Processor;

public class ReactiveTest {
    private ReactiveTest() {
    }

    private static class MySubscriber<T> implements Subscriber<T> {
        private Subscription subscription;

        @Override
        public void onSubscribe(Subscription subscription) {
            this.subscription = subscription;
            subscription.request(1);
        }

        @Override
        public void onNext(T item) {
            System.out.println("Got : " + item);
            subscription.request(1);
        }

        @Override
        public void onError(Throwable t) {
            t.printStackTrace();
        }

        @Override
        public void onComplete() {
            System.out.println("Done");
        }
    }

    private static class MyTransformProcessor<T,R> extends SubmissionPublisher<R> implements Processor<T, R> {

        private Function function;
        private Subscription subscription;

        public MyTransformProcessor(Function<? super T, ? extends R> function) {
            super();
            this.function = function;
        }

        @Override
        public void onSubscribe(Subscription subscription) {
            this.subscription = subscription;
            subscription.request(1);
        }

        @Override
        public void onNext(T item) {
            submit((R) function.apply(item));
            subscription.request(1);
        }

        @Override
        public void onError(Throwable t) {
            t.printStackTrace();
        }

        @Override
        public void onComplete() {
            close();
        }
    }

    private static class MyFilterProcessor<T> extends SubmissionPublisher<T> implements Processor<T, T> {

        private Predicate<? super T> predicate;
        private Subscription subscription;

        public MyFilterProcessor(Predicate<? super T> predicate) {
            super();
            this.predicate = predicate;
        }

        @Override
        public void onSubscribe(Subscription subscription) {
            this.subscription = subscription;
            subscription.request(1);
        }

        @Override
        public void onNext(T item) {
            if (predicate.test(item)) {
                submit(item);
            }
            subscription.request(1);
        }

        @Override
        public void onError(Throwable t) {
            t.printStackTrace();
        }

        @Override
        public void onComplete() {
            close();
        }
    }

    public static void main(String[] args) throws InterruptedException {
        simpleTest();
        processorsTest();
    }

    private static void simpleTest() throws InterruptedException {
        SubmissionPublisher<String> publisher = new SubmissionPublisher<>();

        MySubscriber<String> subscriber = new MySubscriber<>();
        publisher.subscribe(subscriber);

        publishItems(publisher);

        Thread.sleep(5000);
        publisher.close();
    }

    private static void processorsTest() throws InterruptedException {
        SubmissionPublisher<String> publisher = new SubmissionPublisher<>();

        MyFilterProcessor<String> filterProcessor = new MyFilterProcessor<>(s -> !s.equals("x"));

        MyTransformProcessor<String, Integer> transformProcessor = new MyTransformProcessor<>(Integer::parseInt);

        MySubscriber<Integer> subscriber = new MySubscriber<>();

        publisher.subscribe(filterProcessor);
        filterProcessor.subscribe(transformProcessor);
        transformProcessor.subscribe(subscriber);
        
        publishItems(publisher);

        Thread.sleep(5000);
        publisher.close();
    }

    private static void publishItems(SubmissionPublisher<String> publisher) {
        System.out.println("Publishing Items...");
        Stream.of("1", "x", "2", "x", "3", "x").forEach(publisher::submit);
    }
}