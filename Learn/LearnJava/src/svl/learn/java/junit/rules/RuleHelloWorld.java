package svl.learn.java.junit.rules;

import junit.framework.TestCase;
import org.junit.rules.ExternalResource;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;


public class RuleHelloWorld extends ExternalResource {
    @Override
    public Statement apply(Statement base, Description description) {
        return new Statement() {
            @Override
            public void evaluate() throws Throwable {
                System.out.println(description.getDisplayName() + "   " + description.getTestClass());
                description.getChildren().stream().forEach(d-> System.out.println("\t" + d));
                base.evaluate();
            }
        };
    }
}
