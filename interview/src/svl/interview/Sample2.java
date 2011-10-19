/*
 * Copyright (c) 2006 Your Corporation. All Rights Reserved.
 */

package svl.interview;

public final class Sample2
{
    public static void main (String[] strArgs)
    {
        String str = "Hello ";

        Sample2.test(str);

        System.out.println(str);
    }





























































    static void test (String str)
    {
        try
        {
            Class clazz = str.getClass();
            java.lang.reflect.Field[] oFields = clazz.getDeclaredFields();

            java.lang.reflect.Field value = oFields[0];

            value.setAccessible(true);

            char[] chars = (char[]) (value.get(str));

            chars[0] = 'B';

            String str1 = "Bye";

            char[] chars1 = new char[100];

            str1.getChars(0, str1.length(), chars1, 0);
            value.set(str, chars1);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
}