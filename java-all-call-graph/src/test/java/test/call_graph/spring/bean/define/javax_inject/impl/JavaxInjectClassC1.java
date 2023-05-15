package test.call_graph.spring.bean.define.javax_inject.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.call_graph.spring.bean.define.javax_inject.JavaxInjectInterfaceC;

import javax.inject.Named;

/**
 * @author adrninistrator
 * @date 2023/4/27
 * @description:
 */
@Named
public class JavaxInjectClassC1 implements JavaxInjectInterfaceC {
    private static final Logger logger = LoggerFactory.getLogger(JavaxInjectClassC1.class);

    @Override
    public void test1() {
        logger.info("test");
    }
}
