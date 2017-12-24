package svl.example.postgres.json;

public class HandledException extends RuntimeException {
    public HandledException() {
    }

    public HandledException(String message) {
        super(message);
    }

    public HandledException(String message, Throwable cause) {
        super(message, cause);
    }

    public HandledException(Throwable cause) {
        super(cause);
    }
}