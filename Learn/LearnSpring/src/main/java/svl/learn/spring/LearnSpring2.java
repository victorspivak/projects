package svl.learn.spring;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MapPropertySource;
import org.springframework.core.env.MutablePropertySources;

import java.util.HashMap;
import java.util.Map;

/**
 * It is uses an advanced spring config. It combines scanning annotations and explicit configuration Java class.
 * The config values are specified by Map.
 */
public class LearnSpring2 {
    public static void main(String[] args) {
        AnnotationConfigApplicationContext ctx = new AnnotationConfigApplicationContext();

        ConfigurableEnvironment environment = ctx.getEnvironment();
        MutablePropertySources propertySources = environment.getPropertySources();
        Map myMap = new HashMap();
        myMap.put("logging.prefix", ">>>>> ");

        propertySources.addFirst(new MapPropertySource("MY_MAP", myMap));
        ctx.register(AppSpringConfig2.class);
        ctx.refresh();

        for (int i = 0; i < 5; i++) {
            MyProcessor processor = ctx.getBean(MyProcessor.class);

            processor.process("Request " + i);
        }
    }
}