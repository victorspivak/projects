package svl.learn.java.java7.trymonad;

import org.junit.Test;
import svl.learn.java.java7.trymonad.Failure;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.SocketException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public class TryTest {
    @Test public void iterateCollection() {
        List<Integer> data1 = Arrays.asList(1, 2, 3, 0, 4, 5);
        List<Try<Integer>> result1 = data1.stream().map(Try.tryOf(i -> 100 / i)).collect(Collectors.toList());
        assertEquals(Arrays.asList(Try.success(100), Try.success(50), Try.success(33),
            Try.failure(new ArithmeticException("/ by zero")), Try.success(25), Try.success(20)), result1);
        Try<List<Integer>> flattenResult1 = Try.flattenList(result1);
        assertTrue(flattenResult1.isError());

        Try<List<Integer>> result2 = Try.tryOf(() -> data1.stream().map(i -> 100 / i).collect(Collectors.toList()));
        assertEquals(new Failure<List<Integer>>(new ArithmeticException("/ by zero")), result2);

        List<Integer> data2 = Arrays.asList(1, 2, 3, 4, 5);
        Try<List<Integer>> result3 = Try.tryOf(() -> data2.stream().map(i -> 100 / i).collect(Collectors.toList()));
        assertEquals(Try.success(Arrays.asList(100, 50, 33, 25, 20)), result3);

        List<Try<Integer>> result4 = data2.stream().map(Try.tryOf(i -> 100 / i)).collect(Collectors.toList());
        Try<List<Integer>> flattenResult4 = Try.flattenList(result4);
        assertTrue(flattenResult4.isSuccess());
        assertEquals(Try.success(Arrays.asList(100, 50, 33, 25, 20)), flattenResult4);
    }

    @Test public void flatMap() {
        assertLoginInfo(Try.failure(new RuntimeException()), Try.success("victor"), Try.failure(new RuntimeException()), Try.success("password"));
        assertLoginInfo(Try.success("private/victor/password"), Try.success("victor"), Try.success("private"), Try.success("password"));
    }

    @Test public void processError() {
        Set<Integer> isHandlerCalled = Sets.newHashSet();
        int test1 = 1;
        int test2 = 2;

        Try.success("foo").processError(e -> isHandlerCalled.add(test1));
        Try.failure(new IOException()).processError(e -> isHandlerCalled.add(test2));

        assertFalse(isHandlerCalled.contains(test1));
        assertTrue(isHandlerCalled.contains(test2));
    }

    @Test public void orElse() {
        assertEquals("foo", Try.success("foo").orElse("bar"));
        assertEquals("bar", Try.failure(new IOException()).orElse("bar"));

        assertEquals("foo", Try.success("foo").orElse(e -> "bar"));
        assertEquals("bar", Try.failure(new IOException()).orElse(e -> "bar"));
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

        try {
            failure.propagateException(IOException.class).propagateException(InterruptedException.class);
            fail("Expected exception");
        } catch (IOException ignore) {
        } catch (InterruptedException e) {
            fail("Unexpected exception");
        }

        try {
            failure.propagateException(IOException.class, InterruptedException.class);
            fail("Expected exception");
        } catch (IOException ignore) {
        } catch (InterruptedException e) {
            fail("Unexpected exception");
        }

        try {
            failure.propagateException(SocketException.class, IOException.class, InterruptedException.class);
            fail("Expected exception");
        } catch (IOException ignore) {
        } catch (InterruptedException e) {
            fail("Unexpected exception");
        }

        try {
            failure.propagateException(FileNotFoundException.class, SocketException.class, IOException.class, InterruptedException.class);
            fail("Expected exception");
        } catch (IOException ignore) {
        } catch (InterruptedException e) {
            fail("Unexpected exception");
        }

        try {
            failure.propagateException(SQLException.class, FileNotFoundException.class, SocketException.class, IOException.class, InterruptedException.class);
            fail("Expected exception");
        } catch (IOException | SQLException ignore) {
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

        fail("We should not reach this point, because the previous method throw an exception");
    }

    private void assertLoginInfo(Try<String> expected, Try<String> login, Try<String> domain, Try<String> password) {
        Try<String> r = login.flatMap(l -> domain.flatMap(d -> password.map(p -> String.format("%s/%s/%s", d, l, p))));
        assertEquals(expected, r);
    }
}
