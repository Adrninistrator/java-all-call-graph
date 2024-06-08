package test.callgraph.spring.bean.use.wrongname;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.callgraph.spring.bean.define.wrongname.SpringWrongNameService1;
import test.callgraph.spring.bean.define.wrongname.SpringWrongNameService2;

/**
 * @author adrninistrator
 * @date 2023/5/18
 * @description:
 */
@Service
public class TestSpringWrongNameService1 {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringWrongNameService1.class);

    @Autowired
    private SpringWrongNameService1 springWrongNameService2;

    @Autowired
    private SpringWrongNameService2 springWrongNameService1;

    public void test() {
        springWrongNameService2.test();
        springWrongNameService1.test();

        logger.info("1 {}", springWrongNameService2.getClass().getName());
        logger.info("2 {}", springWrongNameService1.getClass().getName());
    }
}
