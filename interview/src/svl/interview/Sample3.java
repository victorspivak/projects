/*
 * Copyright (c) 2006 Your Corporation. All Rights Reserved.
 */

package svl.interview;

public class Sample3
{
    public static void main (String[] args)
    {
        int i   = foo();

        System.out.println("i = " + i);
    }

    private static int foo ()
    {
        try
        {
            return 0;
        }
        finally
        {
            return 1;
        }
    }
}