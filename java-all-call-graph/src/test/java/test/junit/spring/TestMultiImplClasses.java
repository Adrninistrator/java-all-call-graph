package test.junit.spring;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import test.call_graph.spring.bean.use.TestSpringBeanE1;
import test.call_graph.spring.bean.use.TestSpringBeanE2;
import test.call_graph.spring.bean.use.javax_inject.TestJavaxInjectClassA;
import test.call_graph.spring.bean.use.javax_inject.TestJavaxInjectClassB;
import test.call_graph.spring.bean.use.javax_inject.TestJavaxInjectClassC;
import test.junit.base.TestSpringBase;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description:
 */
public class TestMultiImplClasses extends TestSpringBase {

    @Autowired
    private TestSpringBeanE1 testSpringBeanE1;

    @Autowired
    private TestSpringBeanE2 testSpringBeanE2;

    @Autowired
    private TestJavaxInjectClassA testJavaxInjectClassA;

    @Autowired
    private TestJavaxInjectClassB testJavaxInjectClassB;

    @Autowired
    private TestJavaxInjectClassC testJavaxInjectClassC;

    @Test
    public void test() {
        testSpringBeanE1.test();
        testSpringBeanE2.test();

        testJavaxInjectClassA.test();
        testJavaxInjectClassB.test();
        testJavaxInjectClassC.test();
    }
}
