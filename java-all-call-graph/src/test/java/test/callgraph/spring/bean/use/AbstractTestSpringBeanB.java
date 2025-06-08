package test.callgraph.spring.bean.use;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import test.callgraph.spring.bean.define.AbstractSpringServiceC;
import test.callgraph.spring.bean.define.SpringInterfaceB;
import test.callgraph.spring.bean.define.SpringInterfaceD;
import test.callgraph.spring.bean.define.impl.SpringServiceImplB1;
import test.callgraph.spring.bean.define.impl.SpringServiceImplB2;
import test.callgraph.spring.bean.define.impl.SpringServiceImplD1;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
public abstract class AbstractTestSpringBeanB {

    @Autowired
    @Qualifier(SpringServiceImplB1.BEAN_NAME)
    protected SpringInterfaceB springInterfaceB1;

    @Resource(name = SpringServiceImplB2.BEAN_NAME)
    protected SpringInterfaceB springInterfaceB2A;

    @Resource(type = SpringServiceImplB2.class)
    protected SpringInterfaceB springInterfaceB2B;

    @Resource(name = "ThisIsSpringServiceImplC2")
    protected AbstractSpringServiceC springServiceC2;

    @Resource(type = SpringServiceImplD1.class)
    protected SpringInterfaceD springInterfaceD;
}
