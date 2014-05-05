package svl.learn.java.basic;

import java.util.Arrays;
import java.util.List;

@SuppressWarnings({"UtilityClass", "Annotation", "ClassWithoutConstructor", "UtilityClassWithoutPrivateConstructor", "UseOfSystemOutOrSystemErr", "MagicNumber"})
public class TestLambda {
    public static void main(String[] args) {
        simpleExample();

        List<Student> students = Arrays.asList( new Student("S1", 3.2),
                                                new Student("S2", 3.9),
                                                new Student("S3", 2.2),
                                                new Student("S4", 3.7),
                                                new Student("S5", 3.1),
                                                new Student("S10", 0.0));

        //students.stream().filter(s -> s.getGpa() > 3.5).forEach(s -> System.out.println(s));
        students.stream().filter(student -> student.getGpa() > 3.5).map(Student::getName).forEach(System.out::println);
    }

    private static void simpleExample() {
        List<String> features = Arrays.asList("Lambdas", "Default Method", "Stream API", "Date and Time API");

        features.sort((leftString, rightString) -> leftString.compareTo(rightString));
        features.forEach(System.out::println);
    }

    @SuppressWarnings("ClassNamingConvention")
    static private class Student {
        private String name;
        private double gpa;

        Student(String name, double gpa) {
            this.name = name;
            this.gpa = gpa;
        }

        public String getName() {
            return name;
        }

        public double getGpa() {
            return gpa;
        }

        @Override
        public String toString() {
            return "Student{" +
                    "name='" + name + '\'' +
                    ", gpa=" + gpa +
                    '}';
        }
    }
}
