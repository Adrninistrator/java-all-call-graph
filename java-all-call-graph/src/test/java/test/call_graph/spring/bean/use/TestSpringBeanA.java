package test.call_graph.spring.bean.use;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.SpringInterfaceA;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service
public class TestSpringBeanA {

    @Autowired
    private SpringInterfaceA springInterfaceA;

    public void test() {
        springInterfaceA.test1();
    }
}
