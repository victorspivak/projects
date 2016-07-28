package svl.learn.java.basic;

/**
 * TestEnum
 *
 * @author victor.spivak
 * @since 204
 */
public class TestEnum {
    enum Mode {
        On,
        Off;
    }
    public static void main(String[] args) {
        System.out.println("Mode.On + \"  \" + Mode.On.name() = " + Mode.On + "  " + Mode.On.name());
    }
}
