package svl.learn.java.apt.client;

import svl.learn.java.apt.NeedBuilderClass;

@NeedBuilderClass(name = "PersonBuilder")
public class Person {
    private final String firstname;
    private final String lastname;
    private final int age;

    public Person(String firstname, String lastname, int age) {
        this.age = age;
        this.firstname = firstname;
        this.lastname = lastname;
    }

    public int getAge() {
        return age;
    }

    public String getFirstname() {
        return firstname;
    }

    public String getLastname() {
        return lastname;
    }

    @Override
    public String toString() {
        return "Person{" +
                "age=" + age +
                ", firstname='" + firstname + '\'' +
                ", lastname='" + lastname + '\'' +
                '}';
    }
}