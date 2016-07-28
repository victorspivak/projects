package svl.learn.java.junit.rules;

import java.util.logging.Logger;

import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;

public class MyLoggerTest {
    @Rule
    public TestLogger logger = new TestLogger();

//    @Rule public RuleHelloWorld helloWorld = new RuleHelloWorld();
    @ClassRule static public RuleHelloWorld helloWorldClassLevel = new RuleHelloWorld();

    @Test public void checkOutMyLogger() {
        final Logger log = logger.getLogger();
        log.severe("Your test is showing!");
    }

    @Test public void anotherTest() {
        final Logger log = logger.getLogger();
        log.severe("Your test is showing!");
    }
}