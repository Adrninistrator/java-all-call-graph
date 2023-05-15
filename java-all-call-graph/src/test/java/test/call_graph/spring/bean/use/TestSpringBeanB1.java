package test.call_graph.spring.bean.use;

import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service
public class TestSpringBeanB1 extends AbstractTestSpringBeanB {
    public void test() {
        springInterfaceB1.test1();
        springInterfaceB1.test2();

        springInterfaceB2A.test1();

        springServiceC2.test1();
    }
}
