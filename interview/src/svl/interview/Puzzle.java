package svl.interview;

import java.util.ArrayList;
import java.util.List;

public class Puzzle {
    final static String str = "hi";
    public static void main(String[] args) {
      //String str = "hi";

        List<String> list = new ArrayList<String>();
        list.add("hi");
        list.add("Hi");
        System.out.println("str = " + str);
        System.out.println(list);
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
