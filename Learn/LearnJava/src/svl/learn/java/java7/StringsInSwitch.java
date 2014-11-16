package svl.learn.java.java7;

public class StringsInSwitch {
    public static void main(String[] args) {
        test("foo");
        test(null);
    }

    private static void test(String toMatch) {
        if (toMatch != null){
            switch(toMatch) {
                case "blah":System.out.println("blah");break;
                case "foo":System.out.println("foo");break;
                default:System.out.println("oops");break;
            }
        }
    }
}