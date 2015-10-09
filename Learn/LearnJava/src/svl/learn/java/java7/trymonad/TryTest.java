package svl.learn.java.java7.trymonad;

import org.junit.Test;
import svl.learn.java.java7.trymonad.Try.Failure;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class TryTest {
    @Test public void iterateCollection() {
        List<Integer> data1 = Arrays.asList(1, 2, 3, 0, 4, 5);
        List<Try<Integer>> result1 = data1.stream().map(Try.tryOf(i -> 100 / i)).collect(Collectors.toList());
        assertEquals(Arrays.asList(Try.success(100), Try.success(50), Try.success(33),
                Try.failure(new ArithmeticException("/ by zero")), Try.success(25), Try.success(20)), result1);

        Try<List<Integer>> result2 = Try.tryOf(() -> data1.stream().map(i -> 100 / i).collect(Collectors.toList()));
        assertEquals(new Failure<List<Integer>>(new ArithmeticException("/ by zero")), result2);

        List<Integer> data2 = Arrays.asList(1, 2, 3, 4, 5);
        Try<List<Integer>> result3 = Try.tryOf(() -> data2.stream().map(i -> 100 / i).collect(Collectors.toList()));
        assertEquals(Try.success(Arrays.asList(100, 50, 33, 25, 20)), result3);
    }

    @Test public void flatMap() {
        assertLoginInfo(Try.failure(new RuntimeException()), Try.success("victor"), Try.failure(new RuntimeException()), Try.success("password"));    
        assertLoginInfo(Try.success("private/victor/password"), Try.success("victor"), Try.success("private"), Try.success("password"));
    }

    @Test public void testRuntimeException() {
        Try<Object> failure = Try.failure(new NullPointerException());

        try {
            failure.propagateException();
            fail("Expected exception");
        } catch (RuntimeException ignored) {
        }

        try {
            failure.propagateException(IOException.class);
            fail("Expected exception");
        } catch (RuntimeException ignored) {
        } catch (IOException e) {
            fail("Unexpected exception");
        }
    }

    @Test public void testException() {
        Try<Object> failure = Try.failure(new IOException());

        try {
            failure.propagateException(IOException.class);
            fail("Expected exception");
        } catch (IOException ignore) {
        }

        try {
            failure.propagateException(Exception.class);
            fail("Expected exception");
        } catch (Exception ignore) {
        }

        try {
            failure.propagateException(InterruptedException.class);
        } catch (InterruptedException e) {
            fail("Unexpected exception");
        }
    }

    @Test public void testGenericPropagate() {
        try {
            failedMethod();
            fail("Expected IOException");
        } catch (IOException ignore) {
        }
    }

    private void failedMethod() throws IOException {
        Try<Object> failure = Try.failure(new IOException());

        failure.propagateException();
    }

    private void assertLoginInfo(Try<String> expected, Try<String> login, Try<String> domain, Try<String> password) {
        Try<String> r = login.flatMap(l -> domain.flatMap(d -> password.map(p -> String.format("%s/%s/%s", d, l, p))));
        assertEquals(expected, r);
    }
}