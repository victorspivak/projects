package svl.learn.java.junit.dynamic;/*
 * User: victor    Date: 7/30/16   Time: 11:37 PM
 */

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
public @interface TestContext {
    int mode() default 1;
    String backend() default "SOLR";
}
