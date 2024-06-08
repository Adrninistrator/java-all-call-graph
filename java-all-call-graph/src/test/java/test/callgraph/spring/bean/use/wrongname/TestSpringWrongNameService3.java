package test.callgraph.spring.bean.use.wrongname;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.define.wrongname.SpringWrongNameService1;
import test.callgraph.spring.bean.define.wrongname.SpringWrongNameService2;

import javax.inject.Inject;

/**
 * @author adrninistrator
 * @date 2023/5/18
 * @description:
 */
@Service
public class TestSpringWrongNameService3 {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringWrongNameService3.class);

    @Inject
    private SpringWrongNameService1 springWrongNameService2;

    @Inject
    private SpringWrongNameService2 springWrongNameService1;

    public void test() {
        springWrongNameService2.test();
        springWrongNameService1.test();

        logger.info("1 {}", springWrongNameService2.getClass().getName());
        logger.info("2 {}", springWrongNameService1.getClass().getName());
    }
}
