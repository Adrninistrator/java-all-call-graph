package test.call_graph.spring.bean.use;

import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service
public class TestSpringBeanB2B extends AbstractTestSpringBeanB2 {
    public void test() {
        springInterfaceB2A.test2();
    }
}
