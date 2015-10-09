package svl.learn.java.java7.trymonad;

import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.function.Function;

public abstract class Try<V> {
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

    public static <V> Try<V> success(V value) {
        return new Success<>(value);
    }

    public static <V> Try<V> failure(Exception e) {
        return new Failure<>(e);
    }

    public abstract boolean isSuccess();

    abstract public<U> Try<U> map(Function<? super V, ? extends U> mapper);
    abstract public<U> Try<U> flatMap(Function<? super V, Try<U>> mapper);

    public boolean isError() {
        return !isSuccess();
    }

    abstract public V getResult();

    abstract public Exception getError();

    abstract public void propagateException();

    abstract public <E extends Exception> Try<V> propagateException(Class<E> e) throws E;

    public interface ThrowableFunction<I, O> {
        O apply(I a) throws Exception;
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

