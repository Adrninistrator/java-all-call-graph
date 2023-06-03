package test.junit.spring;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import test.call_graph.spring.bean.use.extends_impl.TestSpringExtendsImpl;
import test.junit.base.TestSpringBase;

/**
 * @author adrninistrator
 * @date 2023/5/19
 * @description:
 */
public class TestRunExtendsImpl extends TestSpringBase {

    @Autowired
    private TestSpringExtendsImpl testSpringExtendsImpl;

    @Test
    public void test() {
        testSpringExtendsImpl.test();
    }
}
