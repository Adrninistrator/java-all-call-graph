package test.call_graph.spring.bean.use.wrong_name;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.wrong_name.SpringWrongNameService1;
import test.call_graph.spring.bean.define.wrong_name.SpringWrongNameService2;

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
