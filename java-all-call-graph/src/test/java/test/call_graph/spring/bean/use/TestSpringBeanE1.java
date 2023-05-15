package test.call_graph.spring.bean.use;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.SpringInterfaceE;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description:
 */
@Service
public class TestSpringBeanE1 {

    @Autowired
    private SpringInterfaceE springServiceImplE1;

    public void test() {
        springServiceImplE1.test1();
    }
}
