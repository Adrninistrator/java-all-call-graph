package test.call_graph.spring.bean.use;

import org.springframework.beans.factory.annotation.Autowired;
import test.call_graph.spring.bean.define.AbstractSpringServiceD;
import test.call_graph.spring.bean.define.SpringInterfaceA;
import test.call_graph.spring.bean.define.SpringInterfaceB;
import test.call_graph.spring.bean.define.SpringInterfaceD;
import test.call_graph.spring.bean.define.impl.SpringServiceImplB1;

/**
 * @author adrninistrator
 * @date 2023/3/12
 * @description:
 */
public class TestSpringBeanMulti extends AbstractTestSpringBeanB {

    @Autowired
    private SpringInterfaceA springInterfaceA;

    @Autowired
    private SpringServiceImplB1 springServiceImplB1;

    @Autowired
    private AbstractSpringServiceD springServiceD1;

    public void test() {
        test1(springInterfaceA);

        test2(springServiceImplB1);
        test2(springInterfaceB1);

        test3(springInterfaceB1);
        test3(springInterfaceB1, springInterfaceB2A);
        test3(springInterfaceB1, springInterfaceB2A, springServiceImplB1);

        test4(springServiceD1);
    }

    private void test1(SpringInterfaceA springInterfaceA) {
    }

    private void test2(SpringInterfaceB springInterfaceBArray) {
    }

    private void test3(SpringInterfaceB... springInterfaceBArray) {
    }

    private void test4(SpringInterfaceD... springInterfaceDArray) {
    }
}
