package test.call_graph.spring.bean.use;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import test.call_graph.spring.bean.define.AbstractSpringServiceC;
import test.call_graph.spring.bean.define.SpringInterfaceB;
import test.call_graph.spring.bean.define.SpringInterfaceD;
import test.call_graph.spring.bean.define.impl.SpringServiceImplB2;
import test.call_graph.spring.bean.define.impl.SpringServiceImplD1;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
public abstract class AbstractTestSpringBeanB {

    @Autowired
    @Qualifier("test.call_graph.spring.bean.define.impl.SpringServiceImplB1")
    protected SpringInterfaceB springInterfaceB1;

    @Resource(name = "springServiceImplB2")
    protected SpringInterfaceB springInterfaceB2A;

    @Resource(type = SpringServiceImplB2.class)
    protected SpringInterfaceB springInterfaceB2B;

    @Resource(name = "ThisIsSpringServiceImplC2")
    protected AbstractSpringServiceC springServiceC2;

    @Resource(type = SpringServiceImplD1.class)
    protected SpringInterfaceD springInterfaceD;
}
