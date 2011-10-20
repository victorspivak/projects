/*
 * Copyright, 1999- 2011, SALESFORCE.com 
 * All Rights Reserved
 * Company Confidential
 */
package svl.log.api;

/**
 * Log
 *
 * @author vspivak
 * @since 174
 */
public interface Log {
    public enum LEVEL {
        DEBUG, INFO, WARN, ERROR
    };

    void log(LEVEL level, String message, Throwable t);
}
