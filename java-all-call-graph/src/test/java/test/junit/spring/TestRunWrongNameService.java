package test.junit.spring;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import test.call_graph.spring.bean.use.wrong_name.TestSpringWrongNameService1;
import test.call_graph.spring.bean.use.wrong_name.TestSpringWrongNameService3;
import test.junit.base.TestSpringBase;

/**
 * @author adrninistrator
 * @date 2023/5/18
 * @description:
 */
public class TestRunWrongNameService extends TestSpringBase {

    @Autowired
    private TestSpringWrongNameService1 testSpringWrongNameService1;

//    @Autowired
//    private TestSpringWrongNameService2 testSpringWrongNameService2;

    @Autowired
    private TestSpringWrongNameService3 testSpringWrongNameService3;

    @Test
    public void test() {
        testSpringWrongNameService1.test();
//        testSpringWrongNameService2.test();
        testSpringWrongNameService3.test();
    }
}
