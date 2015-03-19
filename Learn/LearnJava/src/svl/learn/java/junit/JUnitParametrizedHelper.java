package svl.learn.java.junit;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public class JUnitParametrizedHelper {
    public static final int CALLER_STACKTRACE_INDEX = 3;

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.METHOD, ElementType.TYPE})
    public @interface TestDataProvider {
        String method();
    }

    public static class TestDataEntry {
        private String name;
        private Object[] data;

        public TestDataEntry(String name, Object... data) {
            this.name = name;
            this.data = data;
        }

        public String getName() {
            return name;
        }

        public Object[] getData() {
            return data;
        }
    }

    private static final String METHOD_DELIMITER = " : ";

    @SafeVarargs
    public static <T> TestSuite createSuite(T ... params) {
        try {
            Class testClass = extractTestClassFromStacktrace();
            TestSuite suite = new TestSuite(testClass.getName());

            if (params.length > 0) {
                for (T param : params) {
                    processMethods(testClass, suite, true, param);
                }
            } else {
                processMethods(testClass, suite, false, null);
            }

            return suite;
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private static Class extractTestClassFromStacktrace() throws ClassNotFoundException {
        return Class.forName(Thread.currentThread().getStackTrace()[CALLER_STACKTRACE_INDEX].getClassName());
    }

    private static<T> void processMethods(Class testClass, TestSuite suite, boolean useParam, T param) {
        TestClassHandler classInfo = new TestClassHandler(testClass);

        for (Method method : testClass.getMethods()) {
            if (isTestMethod(method)) {
                TestDataProvider dataProvider = getDataProvider(method);
                if (dataProvider != null) {
                    try {
                        String dataProviderMethodName = dataProvider.method();
                        @SuppressWarnings("unchecked") Method dataProviderMethod = testClass.getMethod(dataProviderMethodName);
                        TestDataEntry[] data = (TestDataEntry[]) dataProviderMethod.invoke(testClass);
                        for (TestDataEntry testDataEntry : data) {
                            String testName = buildFullTestName(method, useParam, param, testDataEntry);
                            Object test = createTest(testClass, testName, useParam, param);

                            TestAdopter decoratedTest = new TestAdopter(classInfo, testName, test, method, testDataEntry);

                            suite.addTest(decoratedTest);
                        }

                    } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException e) {
                        throw new RuntimeException(e);
                    }

                } else {
                    String testName = buildFullTestName(method, useParam, param, null);

                    suite.addTest(new TestAdopter(classInfo, testName, createTest(testClass, testName, useParam, param), method));
                }
            }
        }
    }

    private static<T> String buildFullTestName(Method method, boolean useParam, T param, TestDataEntry testDataEntry) {
        if (useParam && testDataEntry != null) {
            return method.getName() + METHOD_DELIMITER + param  + METHOD_DELIMITER + testDataEntry.getName();
        } else if (useParam) {
            return method.getName() + METHOD_DELIMITER + param;
        } else if (testDataEntry != null) {
            return method.getName() + METHOD_DELIMITER + testDataEntry.getName();
        } else {
            return method.getName();
        }
    }

    private static<T> Object createTest(Class testClass, String testName, boolean useParam, T param) {
        final Constructor<?>[] constructors = testClass.getConstructors();
        if (constructors.length != 1) {
            return TestSuite.warning("Class " + testClass.getName() + " should have one constructor " + testClass.getName() + "(String testName, Param)");
        }
        Constructor constructor = constructors[0];
        Object test;

        try {
            if (useParam) {
                test = constructor.newInstance(param);
            } else {
                test = constructor.newInstance();
            }
        } catch (InstantiationException e) {
            return TestSuite.warning("Cannot instantiate test case: " + testName + " (" + e.getMessage() + ")");
        } catch (InvocationTargetException e) {
            return TestSuite.warning("Exception in constructor: " + testName + " (" + e.getMessage() + ")");
        } catch (IllegalAccessException e) {
            return TestSuite.warning("Cannot access test case: " + testName + " (" + e.getMessage() + ")");
        }

        return test;
    }

    /**
     * Figures out if the method is a test case based on a few checks.
     * @return true if the method is a test.
     */
    private static boolean isTestMethod(Method method) {
        boolean isTest = false;

        if (Modifier.isPublic(method.getModifiers()) && !Modifier.isStatic(method.getModifiers())) {
            Class<?> returnType = method.getReturnType();

            if (returnType.equals(Void.TYPE)) {
                String name = method.getName();
                if (method.getAnnotation(Test.class) != null || getDataProvider(method) != null ||
                        (name.startsWith("test") && method.getParameterCount() == 0)) {
                    isTest = true;
                }
            }
        }

        return isTest;
    }

    private static TestDataProvider getDataProvider(Method method) {
        return method.getAnnotation(TestDataProvider.class);
    }

    private static class TestClassHandler {
        private Method before;
        private Method after;

        public TestClassHandler(Class testClass) {
            if (TestCase.class.isAssignableFrom(testClass)) {
                before = getMethod("setUp");
                after = getMethod("tearDown");
            } else {
                for (Method method : testClass.getMethods()) {
                    if (before == null && method.getAnnotation(Before.class) != null){
                        before = method;
                    } else
                    if (after == null && method.getAnnotation(After.class) != null) {
                        after = method;
                    }
                }
            }
        }

        public void before(Object test) {
            invoke(test, before);
        }

        public void after(Object test) {
            invoke(test, after);
        }

        private void invoke(Object test, Method method) {
            try {
                if (method != null) {
                    method.invoke(test);
                }
            } catch (IllegalAccessException | InvocationTargetException e) {
                throw new RuntimeException(e);
            }
        }

        private Method getMethod(String name) {
            try {
                Method method = TestCase.class.getDeclaredMethod(name);
                method.setAccessible(true);
                return method;
            } catch (NoSuchMethodException swallow) {
                return null;
            }
        }
    }

    private static class TestAdopter extends TestCase {
        private TestClassHandler testClassHandler;
        private Object customTest;
        private Method method;
        private TestDataEntry testDataEntry;

        public TestAdopter(TestClassHandler testClassHandler, String name, Object customTest, Method method) {
            this(testClassHandler, name, customTest, method, null);
        }

        public TestAdopter(TestClassHandler testClassHandler, String name, Object customTest, Method method, TestDataEntry testDataEntry) {
            super(name);
            this.customTest = customTest;
            this.method = method;
            this.testDataEntry = testDataEntry;
            this.testClassHandler = testClassHandler;
        }

        @Override
        protected void setUp() throws Exception {
            testClassHandler.before(customTest);
        }

        @Override
        protected void tearDown() throws Exception {
            testClassHandler.after(customTest);
        }

        protected void runTest() throws Throwable {
            try {
                if (testDataEntry != null) {
                    method.invoke(customTest, testDataEntry.getData());
                }
                else{
                    method.invoke(customTest);
                }
            } catch(InvocationTargetException it) {
                throw it.getCause();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }
}
