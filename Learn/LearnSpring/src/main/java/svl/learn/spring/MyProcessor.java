package svl.learn.spring;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope(value = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class MyProcessor {
    private MyLogger logger;
    private ApplicationContext applicationContext;

    @Autowired
    public MyProcessor(ApplicationContext applicationContext, MyLogger logger) {
        this.applicationContext = applicationContext;
        this.logger = logger;
    }

    public void process(String request) {
        logger.log(String.format("process %s", request));

        applicationContext.getBean(MyService.class).serve(String.format("%s: %s", request, Integer.toHexString(hashCode())));
    }
}