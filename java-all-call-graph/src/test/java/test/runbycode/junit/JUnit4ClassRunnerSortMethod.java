package test.runbycode.junit;

import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.TestClass;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/3/17
 * @description:
 */
public class JUnit4ClassRunnerSortMethod extends BlockJUnit4ClassRunner {
    public JUnit4ClassRunnerSortMethod(Class<?> testClass) throws InitializationError {
        super(testClass);
    }

    protected JUnit4ClassRunnerSortMethod(TestClass testClass) throws InitializationError {
        super(testClass);
    }

    @Override
    protected List<FrameworkMethod> computeTestMethods() {
        // super.computeTestMethods()返回的是不可变列表
        List<FrameworkMethod> methods = new ArrayList<>(super.computeTestMethods());
        methods.sort(ComparatorFrameworkMethod.getInstance());
        return methods;
    }
}
