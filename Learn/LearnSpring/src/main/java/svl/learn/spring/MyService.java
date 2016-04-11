package svl.learn.spring;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MyService {
    private MyLogger logger;

    @Autowired
    public MyService(MyLogger logger) {
        this.logger = logger;
    }

    public void serve(String request) {
        logger.log(String.format("Serving %s: %s", request, Integer.toHexString(hashCode())));
    }
}