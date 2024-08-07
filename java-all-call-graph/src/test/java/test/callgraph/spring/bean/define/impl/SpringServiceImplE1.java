package test.callgraph.spring.bean.define.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.define.SpringInterfaceE;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description:
 */
@Service
public class SpringServiceImplE1 implements SpringInterfaceE {
    private static final Logger logger = LoggerFactory.getLogger(SpringServiceImplE1.class);

    @Override
    public void test1() {
        logger.info("test");
    }
}
