package test.call_graph.spring.bean.use;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import test.call_graph.spring.bean.define.AbstractSpringServiceC;
import test.call_graph.spring.bean.define.SpringServiceImplC2;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
public class TestSpringBeanC {

    @Autowired
    @Qualifier("test.call_graph.spring.bean.define.SpringServiceImplC1")
    protected AbstractSpringServiceC springServiceC1;

    @Resource(name = "test.call_graph.spring.bean.define.SpringServiceImplC2")
    protected AbstractSpringServiceC springServiceC2;

    @Resource(name = "test.call_graph.spring.bean.define.SpringServiceImplC2")
    protected SpringServiceImplC2 springServiceImplC2;

    public void test() {
        springServiceC1.test1();
        springServiceC2.test1();
        springServiceImplC2.test2();
    }
}
