package test.call_graph.spring.bean.use;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import test.call_graph.spring.bean.define.AbstractSpringServiceC;
import test.call_graph.spring.bean.define.SpringInterfaceB;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
public abstract class AbstractTestSpringBeanB {

    @Autowired
    @Qualifier("test.call_graph.spring.bean.define.SpringServiceImplB1")
    protected SpringInterfaceB springInterfaceB1;

    @Resource(name = "springServiceImplB2")
    protected SpringInterfaceB springInterfaceB2;

    @Resource(name = "test.call_graph.spring.bean.define.SpringServiceImplC2")
    protected AbstractSpringServiceC springServiceC2;
}
