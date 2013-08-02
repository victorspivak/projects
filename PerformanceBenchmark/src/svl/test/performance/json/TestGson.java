/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.test.performance.json;

import com.google.common.collect.Lists;
import com.google.gson.Gson;

import java.util.List;

public class TestGson {
    public static void main(String[] args) {
        Gson gson = new Gson();
        System.out.println("gson.toJson(1) = " + gson.toJson(1));

        List<String> list   = Lists.newArrayList("Hi", "Hello", "How do you do");
        System.out.println("gson.toJson(1) = " + gson.toJson(list));

        TestBean bean0   = new TestBean("name0", 5, true);
        TestBean bean1   = new TestBean("name1", 1, true);
        TestBean bean2   = new TestBean("name2", 2, true);
        TestBean bean3   = new TestBean("name3", 3, true);
        TestBean bean4   = new TestBean("name4", 4, true);
        TestBean bean5   = new TestBean("name5", 5, true);
        TestBean bean6   = new TestBean("name6", 1, true);
        TestBean bean7   = new TestBean("name7", 2, true);
        TestBean bean8   = new TestBean("name8", 3, true);
        TestBean bean9   = new TestBean("name9", 4, true);

        bean0.setParents(bean1, bean2);
        bean0.setChildren(bean3, bean4, bean5);
        bean4.setChildren(bean5, bean6, bean7);
        bean7.setParents(bean8);
        bean7.setChildren(bean9);

        System.out.println("" + gson.toJson(bean0));

        long    start   = System.currentTimeMillis();
        for (int i = 0; i < 100_000; i++) {
            String json = gson.toJson(bean0);
        }
        long time = System.currentTimeMillis() - start;
        System.out.println("time = " + time);
    }
}