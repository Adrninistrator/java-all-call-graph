package test.junit.spring;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import test.call_graph.spring.bean.use.complex.TestUseComplexService;
import test.junit.base.TestSpringBase;

/**
 * @author adrninistrator
 * @date 2023/5/31
 * @description:
 */
public class TestRunComplexService extends TestSpringBase {

    @Autowired
    private TestUseComplexService testUseComplexService;

    @Test
    public void test() {
        testUseComplexService.test();
    }
}
