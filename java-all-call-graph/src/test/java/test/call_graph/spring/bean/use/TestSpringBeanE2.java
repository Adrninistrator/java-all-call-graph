package test.call_graph.spring.bean.use;

import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.SpringInterfaceE;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description:
 */
@Service
public class TestSpringBeanE2 {

    @Resource
    private SpringInterfaceE springServiceImplE2;

    public void test() {
        springServiceImplE2.test1();
    }
}
