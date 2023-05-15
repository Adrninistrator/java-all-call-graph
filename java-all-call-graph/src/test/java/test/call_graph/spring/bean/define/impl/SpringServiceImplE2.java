package test.call_graph.spring.bean.define.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.SpringInterfaceE;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description:
 */
@Service
public class SpringServiceImplE2 implements SpringInterfaceE {
    private static final Logger logger = LoggerFactory.getLogger(SpringServiceImplE2.class);

    @Override
    public void test1() {
        logger.info("test");
    }
}
