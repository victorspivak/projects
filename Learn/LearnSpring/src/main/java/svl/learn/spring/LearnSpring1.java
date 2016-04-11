package svl.learn.spring;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * It is uses a simple spring config. It combines scanning annotations and explicit configuration Java class.
 * The config values are specified by a property file - see AppSpringConfig1
 */
public class LearnSpring1 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext ctx = new AnnotationConfigApplicationContext(AppSpringConfig1.class);

        for (int i = 0; i < 5; i++) {
            MyProcessor processor = ctx.getBean(MyProcessor.class);

            processor.process("Request " + i);
        }
    }
}