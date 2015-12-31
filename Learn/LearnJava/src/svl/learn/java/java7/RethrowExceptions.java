package svl.learn.java.java7;

public class RethrowExceptions {
    public static void main(String[] args) {
        try {
            rethrowException("First");
        } catch (FirstException | SecondException e) {
            System.out.println(e.getClass().getName());
        }
    }

    private static class FirstException extends Exception { }
    private static class SecondException extends Exception { }

    public static void rethrowException(String exceptionName) throws FirstException, SecondException {
        //noinspection OverlyBroadCatchBlock
        try {
            if (exceptionName.equals("First")) {
                throw new FirstException();
            } else {
                throw new SecondException();
            }
        } catch (Exception e) {
            throw e;
        }
    }
}