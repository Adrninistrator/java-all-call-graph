package test.junit.spring;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import test.callgraph.customflow.methodcallargs.service.TestCFMCAService1;
import test.callgraph.customflow.methodcallargs.service.TestCFMCAService2;
import test.junit.base.TestSpringBase;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description:
 */
public class TestSPRunCFMCAService1 extends TestSpringBase {
    @Autowired
    private TestCFMCAService1 testCFMCAService1;

    @Autowired
    private TestCFMCAService2 testCFMCAService2;

    @Test
    public void test() {
        testCFMCAService1.start();
        testCFMCAService2.start();
    }
}
