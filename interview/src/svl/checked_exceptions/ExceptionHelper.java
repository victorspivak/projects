package svl.checked_exceptions;

public class ExceptionHelper {
    public static void doThrow(Exception e) {
        ExceptionHelper.<RuntimeException> doThrow0(e);
    }

    @SuppressWarnings("unchecked")
    static <E extends Exception> void doThrow0(Exception e) throws E {
        throw (E) e;
    }
}
