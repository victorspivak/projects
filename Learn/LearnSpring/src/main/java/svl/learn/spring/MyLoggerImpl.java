package svl.learn.spring;

import org.springframework.beans.factory.annotation.Value;

public class MyLoggerImpl implements MyLogger {
    @Value("${logging.prefix}") private String prefix;

    public void log(String msg) {
        System.out.println(String.format("%s%s: %s", prefix, msg, Integer.toHexString(hashCode())));
    }
}