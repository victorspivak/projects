package svl.learn.java.apt.client;

public class MainApp {
    public static void main(String[] args) {
        PersonBuilder builder = new PersonBuilder();

        builder.setFirstname("Vic").setLastname("Spivak").setAge(10);

        System.out.println(builder.build());


    }
}