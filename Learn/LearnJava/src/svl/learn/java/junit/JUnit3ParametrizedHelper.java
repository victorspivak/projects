package svl.learn.java.junit;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * JUnitParametrizedHelper
 *
 * @author victor.spivak
 * @since 196
 */
public class JUnit3ParametrizedHelper {

    private static final String METHOD_DELIMITER = " : ";

    public static class ParamEntry<T> {
        final private String name;
        final private T param;

        public ParamEntry(String name, T param) {
            this.name = name;
            this.param = param;
        }

        public String getName() {
            return name;
        }

        public T getParam() {
            return param;
        }
    }

    @SafeVarargs
    public static <T> TestSuite createSuite(Class<? extends TestCase> testClass, ParamEntry<T> ... params) {
        TestSuite suite = new TestSuite(testClass.getName());

        for (ParamEntry<T> param : params) {
            for (Method method : testClass.getMethods()) {
                if (isTestMethod(method)) {
                    suite.addTest(createTest(testClass, buildFullTestName(method, param.getName()), param.getParam()));
                }
            }
        }

        return suite;
    }

    @SafeVarargs
    public static <T> TestSuite createSuite(Class<? extends TestCase> testClass, T ... params) {
        TestSuite suite = new TestSuite(testClass.getName());

        for (T param : params) {
            for (Method method : testClass.getMethods()) {
                if (isTestMethod(method)) {
                    suite.addTest(createTest(testClass, buildFullTestName(method, param.toString()), param));
                }
            }
        }

        return suite;
    }

    public static void runTest(TestCase test) throws Throwable {
         String testName = test.getName();
         Method method = test.getClass().getMethod(extractTestName(testName));

         try {
             if (method != null && isTestMethod(method))
                 method.invoke(test);
         } catch(InvocationTargetException it) {
             throw it.getCause();
         } catch (Exception e) {
             throw new RuntimeException(e);
         }
     }

    private static String extractTestName(String testName) {
        return testName.substring(0, testName.indexOf(METHOD_DELIMITER));
    }

    private static <T> String buildFullTestName(Method method, String name) {
        return method.getName() + METHOD_DELIMITER + name;
    }

    private static<T> Test createTest(Class<? extends TestCase> testClass, String testName, T param) {
        final Constructor<?>[] constructors = testClass.getConstructors();
        if (constructors.length != 1) {
            return TestSuite.warning("Class " + testClass.getName() + " should have one constructor " + testClass.getName() + "(String testName, Param)");
        }
        Constructor constructor = constructors[0];
        Object test;

        try {
            test = constructor.newInstance(testName, param);
        } catch (InstantiationException e) {
            return TestSuite.warning("Cannot instantiate test case: " + testName + " (" + e.getMessage() + ")");
        } catch (InvocationTargetException e) {
            return TestSuite.warning("Exception in constructor: " + testName + " (" + e.getMessage() + ")");
        } catch (IllegalAccessException e) {
           return TestSuite.warning("Cannot access test case: " + testName + " (" + e.getMessage() + ")");
        }

        return (Test)test;
    }

    /**
     * Figures out if the method is a test case based on a few checks.
     * @return true if the method is a test.
     */
    private static boolean isTestMethod(Method method) {
        String name = method.getName();
        Class<?>[] parameters = method.getParameterTypes();
        Class<?> returnType = method.getReturnType();
        return Modifier.isPublic(method.getModifiers()) &&
                parameters.length == 0 &&
                name.startsWith("test") &&
                returnType.equals(Void.TYPE);
    }
}
