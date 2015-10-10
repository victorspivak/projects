package svl.learn.java.junit;

import org.junit.Before;

public class MyBaseTest {
    private boolean init;

    @Before
    public void before(){
        init = true;
    }

    public boolean getInit() {
        return init;
    }
}
