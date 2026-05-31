package test.junit.spring;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import test.callgraph.spring.custommethodcall.TestController;
import test.junit.base.TestSpringBase;

/**
 * @author adrninistrator
 * @date 2026/5/25
 * @description:
 */
public class TestSPCustomMethodCall extends TestSpringBase {

    @Autowired
    private TestController testController;

    @Test
    public void test() {
        testController.serviceA("1", "2", "-");
    }
}
