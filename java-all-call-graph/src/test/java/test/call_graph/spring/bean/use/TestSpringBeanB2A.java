package test.call_graph.spring.bean.use;

import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service
public class TestSpringBeanB2A extends AbstractTestSpringBeanB2 {
    public void test() {
        springInterfaceB2A.test1();
        springInterfaceB2B.test1();
        springInterfaceD.test1();
    }
}
