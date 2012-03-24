/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.test.performance.json;

import com.google.common.collect.Lists;

import java.util.List;

public class TestBean {
    private String name;
    private int age;
    private boolean isMarried;
    private List<TestBean> parents;
    private List<TestBean> children;

    public TestBean(String name, int age, boolean married) {
        this.name = name;
        this.age = age;
        isMarried = married;
    }

    public List<TestBean> getParents() {
        return parents;
    }

    public void setParents(TestBean... parents) {
        this.parents = Lists.newArrayList(parents);
    }

    public List<TestBean> getChildren() {
        return children;
    }

    public void setChildren(TestBean... children) {
        this.children = Lists.newArrayList(children);
    }

    public String getName() {
        return name;
    }

    public int getAge() {
        return age;
    }

    public boolean isMarried() {
        return isMarried;
    }

    @Override
    public String toString() {
        return "TestBean{" +
                "name='" + name + '\'' +
                ", age=" + age +
                ", isMarried=" + isMarried +
                '}';
    }
}