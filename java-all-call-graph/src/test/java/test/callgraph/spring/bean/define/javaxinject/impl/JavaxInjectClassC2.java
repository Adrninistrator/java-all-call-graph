package test.callgraph.spring.bean.define.javaxinject.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.spring.bean.define.javaxinject.JavaxInjectInterfaceC;

import javax.inject.Named;

/**
 * @author adrninistrator
 * @date 2023/4/27
 * @description:
 */
@Named
public class JavaxInjectClassC2 implements JavaxInjectInterfaceC {
    private static final Logger logger = LoggerFactory.getLogger(JavaxInjectClassC2.class);

    @Override
    public void test1() {
        logger.info("test");
    }
}
