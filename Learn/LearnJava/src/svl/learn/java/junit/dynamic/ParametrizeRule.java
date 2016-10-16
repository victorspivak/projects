package svl.learn.java.junit.dynamic;

import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.Collections;

public class ParametrizeRule implements TestRule {
    @Override
    public Statement apply(Statement statement, Description desc) {
        Collection<Annotation> annotations = desc.getAnnotations();
        Description newDesc = Description.createTestDescription(desc.getTestClass(),
                desc.getDisplayName() + "_1",
                annotations.toArray(new Annotation[annotations.size()]));

        desc.addChild(newDesc);
        desc.addChild(newDesc);
        desc.addChild(newDesc);

        return statement;
    }
}