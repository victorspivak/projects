/*
 * Copyright (c) 2006 Your Corporation. All Rights Reserved.
 */

package svl.interview;

public class Sample1
{

    public static void main(String args[])
    {
        try
        {
            Thread  t   = null;

            System.out.println("Start sleeping");
            t.sleep(1000);
            System.out.println("End sleeping");
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
}