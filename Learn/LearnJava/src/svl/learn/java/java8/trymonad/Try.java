package svl.learn.java.java8.trymonad;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Try monad (see scala).
 */
public abstract class Try<V> {

    /**
     * Factory method that creates Try monad.
     * @param function which might fail
     * @param <V> expected value type
     * @return success or failure Try instance.
     */
    public static <V> Try<V> tryOf(Callable<V> function) {
        try {
            V result = function.call();
            return success(result);
        } catch (Exception e) {
            return failure(e);
        }
    }

    public static <I, O> Function<I, Try<O>> tryOf(ThrowableFunction<I, O> function) {
        return a -> tryOf(() -> function.apply(a));
    }

    /**
     * Constructs success Try instance based on passed value
     */
    public static <V> Try<V> success(V value) {
        return new Success<>(value);
    }

    /**
     * Constructs failure Try instance based on passed exception
     */
    public static <V> Try<V> failure(Exception e) {
        return new Failure<>(e);
    }

    /**
     * @return true if it is a success instance
     */
    public abstract boolean isSuccess();

    /**
     * If it is a success instance, apply the provided mapping function to it
     */
    abstract public<U> Try<U> map(Function<? super V, ? extends U> mapper);

    /**
     * If it is a success instance, apply the provided Try-bearing mapping function to it
     */
    abstract public<U> Try<U> flatMap(Function<? super V, Try<U>> mapper);

    /**
     * If it is a failure instance, apply supplied handler to the stored exception
     */
    abstract public Try<V> processError(Consumer<? super Exception> handler);

    /**
     * @return true if it is a failure instance
     */
    public boolean isError() {
        return !isSuccess();
    }

    /**
     * @return result in a case of success instance, otherwise it throws UnsupportedOperationException.
     */
    abstract public V getResult();

    /**
     * @return stored exception in a case of failure instance, otherwise it throws UnsupportedOperationException.
     */
    abstract public Exception getError();

    /**
     * In a case of failure instance, it throws any stored exception
     */
    abstract public void propagateException();

    /**
     * The following methods family throws only exceptions specified as parameters or any RuntimeException
     */
    abstract public <E extends Exception> Try<V> propagateException(Class<E> e) throws E;
    abstract public <E1 extends Exception, E2 extends Exception> Try<V>
                propagateException(Class<E1> e1, Class<E2> e2) throws E1, E2;
    abstract public <E1 extends Exception, E2 extends Exception, E3 extends Exception> Try<V>
                propagateException(Class<E1> e1, Class<E2> e2, Class<E3> e3) throws E1, E2, E3;
    abstract public <E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception> Try<V>
                propagateException(Class<E1> e1, Class<E2> e2, Class<E3> e3, Class<E4> e4) throws E1, E2, E3, E4;
    abstract public <E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception, E5 extends Exception> Try<V>
                propagateException(Class<E1> e1, Class<E2> e2, Class<E3> e3, Class<E4> e4, Class<E5> e5) throws E1, E2, E3, E4, E5;

    /**
     * Returns a stored value for success instance, otherwise returns the supplied defaultValue
     */
    abstract public V orElse(V defaultValue);

    /**
     * Returns a stored value for success instance, otherwise returns result of the handler return value
     */
    abstract public V orElse(Function<Exception, ? extends V> handler);

    public interface ThrowableFunction<I, O> {
        O apply(I a) throws Exception;
    }

    /**
     * The flattenList function converts List of Try objects into Try of list objects. If the source list has at least
     * one failure then the result is failure and it will take the first found exception
     */
    public static <V> Try<List<V>> flattenList(List<Try<V>> list) {
        List<V> resultList = list.stream().filter(Try::isSuccess).map(Try::getResult).collect(Collectors.toList());

        return list.size() == resultList.size() ? Try.success(resultList) : Try.failure(
                list.stream().filter(Try::isError).limit(1).collect(Collectors.toList()).get(0).getError()
        );
    }

    static public class Success<V> extends Try<V> {

        private final V result;

        public Success(V result) {
            this.result = result;
        }

        @Override
        public<U> Try<U> map(Function<? super V, ? extends U> mapper) {
            Objects.requireNonNull(mapper);
            return tryOf(() -> mapper.apply(getResult()));
        }

        @Override
        public<U> Try<U> flatMap(Function<? super V, Try<U>> mapper) {
            Objects.requireNonNull(mapper);
            return mapper.apply(getResult());
        }

        @Override
        public Try<V> processError(Consumer<? super Exception> handler) {
            return this;
        }

        @Override
        public boolean isSuccess() {
            return true;
        }

        @Override
        public V getResult() {
            return result;
        }

        @Override
        public Exception getError() {
            return new UnsupportedOperationException();
        }

        @Override
        public void propagateException() {
        }

        @Override
        public <E extends Exception> Try<V> propagateException(Class<E> e) throws E {
            return this;
        }

        @Override
        public <E1 extends Exception, E2 extends Exception> Try<V> propagateException(Class<E1> e1, Class<E2> e2) throws E1, E2 {
            return this;
        }

        @Override
        public <E1 extends Exception, E2 extends Exception, E3 extends Exception> Try<V>
                    propagateException(Class<E1> e1, Class<E2> e2, Class<E3> e3) throws E1, E2, E3 {
            return this;
        }

        @Override
        public <E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception> Try<V>
                    propagateException(Class<E1> e1, Class<E2> e2, Class<E3> e3, Class<E4> e4) throws E1, E2, E3, E4 {
            return this;
        }

        @Override
        public <E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception, E5 extends Exception> Try<V>
                    propagateException(Class<E1> e1, Class<E2> e2, Class<E3> e3, Class<E4> e4, Class<E5> e5) throws E1, E2, E3, E4, E5 {
            return this;
        }

        @Override
        public V orElse(V defaultValue) {
            return result;
        }

        @Override
        public V orElse(Function<Exception, ? extends V> handler) {
            return result;
        }

        @Override
        public boolean equals(Object that) {
            return that instanceof Success && Objects.equals(result, ((Success) that).getResult());
        }

        @Override
        public int hashCode() {
            return result != null ? result.hashCode() : 0;
        }

        @Override
        public String toString() {
            return "Success{" + result + '}';
        }
    }

    static public class Failure<V> extends Try<V> {

        private final Exception exception;

        public Failure(Exception exception) {
            this.exception = exception;
        }

        @Override
        public<U> Try<U> map(Function<? super V, ? extends U> mapper) {
            return failure(getError());
        }

        @Override
        public<U> Try<U> flatMap(Function<? super V, Try<U>> mapper) {
            Objects.requireNonNull(mapper);
            return failure(getError());
        }

        @Override
        public Try<V> processError(Consumer<? super Exception> handler) {
            handler.accept(exception);

            return this;
        }

        @Override
        public boolean isSuccess() {
            return false;
        }

        @Override
        public V getResult() {
            throw new UnsupportedOperationException();
        }

        @Override
        public Exception getError() {
            return exception;
        }

        @Override
        public void propagateException() {
            ExceptionHelper.doThrow(exception);
        }

        @Override
        public <E extends Exception> Try<V> propagateException(Class<E> e) throws E {
            if (exception instanceof RuntimeException || e.isInstance(exception))
                ExceptionHelper.doThrow(exception);
            return this;
        }

        @Override
        public <E1 extends Exception, E2 extends Exception> Try<V> propagateException(Class<E1> e1, Class<E2> e2) throws E1, E2 {
            propagateException(e1);
            propagateException(e2);

            return this;
        }

        @Override
        public <E1 extends Exception, E2 extends Exception, E3 extends Exception> Try<V>
                    propagateException(Class<E1> e1, Class<E2> e2, Class<E3> e3) throws E1, E2, E3 {
            propagateException(e1);
            propagateException(e2, e3);

            return this;
        }

        @Override
        public <E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception> Try<V>
                    propagateException(Class<E1> e1, Class<E2> e2, Class<E3> e3, Class<E4> e4) throws E1, E2, E3, E4 {
            propagateException(e1);
            propagateException(e2, e3, e4);

            return this;
        }

        @Override
        public <E1 extends Exception, E2 extends Exception, E3 extends Exception, E4 extends Exception, E5 extends Exception> Try<V>
                    propagateException(Class<E1> e1, Class<E2> e2, Class<E3> e3, Class<E4> e4, Class<E5> e5) throws E1, E2, E3, E4, E5 {
            propagateException(e1);
            propagateException(e2, e3, e4, e5);

            return this;
        }

        @Override
        public V orElse(V defaultValue) {
            return defaultValue;
        }

        @Override
        public V orElse(Function<Exception, ? extends V> handler) {
            return handler.apply(exception);
        }

        @Override
        public int hashCode() {
            return exception.hashCode();
        }

        @Override
        public boolean equals(Object that) {
            return that instanceof Failure &&
                    exception.getClass().equals(((Failure) that).getError().getClass()) &&
                    Objects.equals(exception.getMessage(), ((Failure) that).getError().getMessage());
        }

        @Override
        public String toString() {
            return "Failure{" + exception.getClass().getName() + " : " + exception.getMessage() + '}';
        }
    }

    static private class ExceptionHelper {
        public static void doThrow(Exception e) {
            ExceptionHelper.<RuntimeException> throwException(e);
        }

        @SuppressWarnings("unchecked")
        static <E extends Exception> void throwException(Exception e) throws E {
            throw (E) e;
        }
    }
}

