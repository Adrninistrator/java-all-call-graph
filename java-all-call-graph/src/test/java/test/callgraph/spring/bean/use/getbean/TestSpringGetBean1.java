package test.callgraph.spring.bean.use.getbean;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.define.SpringInterfaceB;
import test.callgraph.spring.bean.define.complex.TestComplexInterface;
import test.callgraph.spring.bean.define.complex.TestComplexServiceImpl1;
import test.callgraph.spring.bean.define.impl.SpringServiceImplB1;
import test.callgraph.spring.bean.define.impl.SpringServiceImplB2;
import test.junit.base.TestSpringBase;

/**
 * @author adrninistrator
 * @date 2025/3/23
 * @description:
 */
@Service
public class TestSpringGetBean1 extends TestSpringBase implements ApplicationContextAware {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringGetBean1.class);
    private ApplicationContext applicationContext;

    // 参数使用父类/接口类型
    @Test
    public void test1() {
        TestComplexInterface testComplexInterface = applicationContext.getBean(TestComplexInterface.class);
        printBeanName(testComplexInterface);
        testComplexInterface.test1();
    }

    // 参数使用子类/实现类类型
    @Test
    public void test2a() {
        TestComplexInterface testComplexInterface = applicationContext.getBean(TestComplexServiceImpl1.class);
        printBeanName(testComplexInterface);
        testComplexInterface.test1();
    }

    // 参数使用子类/实现类名称+子类/实现类类型
    @Test
    public void test2b() {
        TestComplexInterface testComplexInterface = applicationContext.getBean(TestComplexServiceImpl1.BEAN_NAME, TestComplexServiceImpl1.class);
        printBeanName(testComplexInterface);
        testComplexInterface.test1();
    }

    // 参数使用子类/实现类名称+父类/接口类型
    @Test
    public void test2c() {
        TestComplexInterface testComplexInterface = applicationContext.getBean(TestComplexServiceImpl1.BEAN_NAME, TestComplexInterface.class);
        printBeanName(testComplexInterface);
        testComplexInterface.test1();
    }

    // 参数使用子类/实现类类型，且变量类型使用子类/实现类类型
    @Test
    public void test2d() {
        TestComplexServiceImpl1 testComplexInterface = applicationContext.getBean(TestComplexServiceImpl1.class);
        printBeanName(testComplexInterface);
        testComplexInterface.test1();
    }

    // 参数使用子类/实现类名称，需要强制类型转换
    @Test
    public void test3() {
        TestComplexInterface complexInterface = (TestComplexInterface) applicationContext.getBean(TestComplexServiceImpl1.BEAN_NAME);
        printBeanName(complexInterface);
        complexInterface.test1();
    }

    @Test
    public void testMulti1a() {
        boolean use1 = System.currentTimeMillis() % 2 == 1;
        SpringInterfaceB springInterfaceB = use1 ? (SpringInterfaceB) applicationContext.getBean(SpringServiceImplB1.BEAN_NAME) :
                (SpringInterfaceB) applicationContext.getBean(SpringServiceImplB2.BEAN_NAME);
        printBeanName(springInterfaceB);
        springInterfaceB.test1();
    }

    @Test
    public void testMulti1b() {
        boolean use1 = System.currentTimeMillis() % 2 == 1;
        SpringInterfaceB springInterfaceB = (SpringInterfaceB) (use1 ? applicationContext.getBean(SpringServiceImplB1.BEAN_NAME) :
                applicationContext.getBean(SpringServiceImplB2.BEAN_NAME));
        printBeanName(springInterfaceB);
        springInterfaceB.test1();
    }

    @Test
    public void testMulti1c() {
        boolean use1 = System.currentTimeMillis() % 2 == 1;
        SpringInterfaceB springInterfaceB = (SpringInterfaceB) applicationContext.getBean(use1 ? SpringServiceImplB1.BEAN_NAME : SpringServiceImplB2.BEAN_NAME);
        printBeanName(springInterfaceB);
        springInterfaceB.test1();
    }

    @Test
    public void testMulti2a() {
        boolean use1 = System.currentTimeMillis() % 2 == 1;
        SpringInterfaceB springInterfaceB = use1 ? applicationContext.getBean(SpringServiceImplB1.BEAN_NAME, SpringInterfaceB.class) :
                applicationContext.getBean(SpringServiceImplB2.BEAN_NAME, SpringInterfaceB.class);
        printBeanName(springInterfaceB);
        springInterfaceB.test1();
    }

    @Test
    public void testMulti2b() {
        boolean use1 = System.currentTimeMillis() % 2 == 1;
        SpringInterfaceB springInterfaceB = applicationContext.getBean(use1 ? SpringServiceImplB1.BEAN_NAME : SpringServiceImplB2.BEAN_NAME, SpringInterfaceB.class);
        printBeanName(springInterfaceB);
        springInterfaceB.test1();
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    private void printBeanName(Object object) {
        logger.info("bean name {}", object.getClass().getName());
    }
}
