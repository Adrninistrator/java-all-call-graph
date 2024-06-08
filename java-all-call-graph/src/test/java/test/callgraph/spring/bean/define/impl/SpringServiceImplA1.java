package test.callgraph.spring.bean.define.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.define.SpringInterfaceA;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
@Service("test.callgraph.spring.bean.define.impl.SpringServiceImplA1")
public class SpringServiceImplA1 implements SpringInterfaceA {
    private static final Logger logger = LoggerFactory.getLogger(SpringServiceImplA1.class);

    @Override
    public void test1() {
        logger.info("run {} {}", this.getClass().getName(), System.identityHashCode(this));
        System.getProperty(this.getClass().getName());
    }

    @Override
    public String test2() {
        return null;
    }
}
