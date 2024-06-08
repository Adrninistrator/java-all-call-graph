package test.callgraph.spring.bean.define.javaxinject.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.spring.bean.define.javaxinject.JavaxInjectInterfaceA;

import javax.inject.Named;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description:
 */
@Named
public class JavaxInjectClassA2 implements JavaxInjectInterfaceA {
    private static final Logger logger = LoggerFactory.getLogger(JavaxInjectClassA2.class);

    @Override
    public void test1() {
        logger.info("test");
    }
}
