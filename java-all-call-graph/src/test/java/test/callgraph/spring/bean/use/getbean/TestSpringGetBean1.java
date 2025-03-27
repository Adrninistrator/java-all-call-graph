package test.callgraph.spring.bean.use.getbean;

import org.junit.Test;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.define.complex.TestComplexInterface;
import test.callgraph.spring.bean.define.complex.TestComplexServiceImpl1;
import test.junit.base.TestSpringBase;

/**
 * @author adrninistrator
 * @date 2025/3/23
 * @description:
 */
@Service
public class TestSpringGetBean1 extends TestSpringBase implements ApplicationContextAware {

    private ApplicationContext applicationContext;

    @Test
    public void test1() {
        TestComplexInterface testComplexInterface = applicationContext.getBean(TestComplexInterface.class);
        testComplexInterface.test1();
    }

    @Test
    public void test2() {
        TestComplexInterface testComplexInterface = applicationContext.getBean(TestComplexServiceImpl1.class);
        testComplexInterface.test1();
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }
}
