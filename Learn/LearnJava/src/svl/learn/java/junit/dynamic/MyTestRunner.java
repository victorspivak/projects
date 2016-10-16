package svl.learn.java.junit.dynamic;

import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.Statement;

public class MyTestRunner extends BlockJUnit4ClassRunner {
    public MyTestRunner(Class<?> klass) throws InitializationError {
        super(klass);
        System.out.println("klass = " + klass);
    }


    @Override
    protected Statement methodBlock(FrameworkMethod method) {
        return super.methodBlock(method);    //SVL
    }
}