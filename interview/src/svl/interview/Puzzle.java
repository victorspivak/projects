package svl.interview;

public class Puzzle {
    public static void main(String[] args) {
        String str = "hi";

        System.out.println("It is a test for Alex: str = " + str);
    }




































































































































    static {
        String strOld = "hi";
        String newStr = "bye";

        Class clazz = strOld.getClass();

        java.lang.reflect.Field[] fields = clazz.getDeclaredFields();

        for (java.lang.reflect.Field field : fields) {
            int modifiers = field.getModifiers();

            if (!java.lang.reflect.Modifier.isStatic(modifiers)) {
                field.setAccessible(true);

                try {
                    field.set(strOld, field.get(newStr));
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
