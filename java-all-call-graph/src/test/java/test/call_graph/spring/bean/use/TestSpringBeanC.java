package test.call_graph.spring.bean.use;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.AbstractSpringServiceC;
import test.call_graph.spring.bean.define.SpringInterfaceC;
import test.call_graph.spring.bean.define.impl.SpringServiceImplC2;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service
public class TestSpringBeanC {

    @Autowired
    @Qualifier("test.call_graph.spring.bean.define.impl.SpringServiceImplC1")
    protected AbstractSpringServiceC springServiceC1;

    @Resource(type = SpringServiceImplC2.class)
    protected AbstractSpringServiceC springServiceC2A;

    @Resource(type = SpringServiceImplC2.class)
    protected SpringInterfaceC springServiceC2B;

    @Resource(name = "ThisIsSpringServiceImplC2")
    protected SpringInterfaceC springServiceC2C;

    @Resource(name = "ThisIsSpringServiceImplC2")
    protected SpringServiceImplC2 springServiceC2D;

    public void test() {
        springServiceC1.test1();
        springServiceC2A.test1();
        springServiceC2B.test1();
        springServiceC2C.test2();
        springServiceC2D.test2();
    }
}
