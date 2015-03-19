package svl.learn.java.junit;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * JUnitParametrizedHelper
 *
 * @author victor.spivak
 * @since 196
 */
public class JUnitParametrizedHelper {
    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.METHOD})
    public @interface TestDataProvider {
        String name();
    }


}
