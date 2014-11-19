package svl.learn.java.java7.trymonad;

import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.function.Function;

public abstract class Try<A> {
    public static boolean isSuccess(Try tryy) {
        return tryy instanceof Success;
    }

    public static <A> Try<A> tryOf(Callable<A> function) {
        try {
            A result = function.call();
            return new Success<A>(result);
        } catch (Exception e) {
            return new Failure<>(e);
        }
    }

    public static <A, B> Function<A, Try<B>> tryOf(ThrowableFunction<A, B> function) {
        return a -> {
            try {
                B result = function.apply(a);
                return new Success<B>(result);
            } catch (Exception e) {
                return new Failure<>(e);
            }
        };
    }

    public abstract boolean isSuccess();

    public boolean isError() {
        return !isSuccess();
    }

    public abstract A getResult();

    public abstract Exception getError();

    public interface ThrowableFunction<A, B> {
        B apply(A a) throws Exception;
    }

    static public class Success<A> extends Try<A> {

        private final A result;

        public Success(A result) {
            this.result = result;
        }

        @Override
        public boolean isSuccess() {
            return true;
        }

        @Override
        public A getResult() {
            return result;
        }

        @Override
        public Exception getError() {
            return new UnsupportedOperationException();
        }

        @Override
        public boolean equals(Object that) {
            if(!(that instanceof Success)) {
                return false;
            }
            return Objects.equals(result, ((Success) that).getResult());
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

    static public class Failure<A> extends Try<A> {

        private final Exception exception;

        public Failure(Exception exception) {
            this.exception = exception;
        }

        @Override
        public boolean isSuccess() {
            return false;
        }

        @Override
        public A getResult() {
            throw new UnsupportedOperationException();
        }

        @Override
        public Exception getError() {
            return exception;
        }

        @Override
        public String toString() {
            return "Failure{" + exception.getMessage() + '}';
        }
    }
}

