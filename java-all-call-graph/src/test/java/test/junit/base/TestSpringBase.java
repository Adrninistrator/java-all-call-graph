package test.junit.base;

import org.junit.Before;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author adrninistrator
 * @date 2022/10/4
 * @description:
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:applicationContext.xml"})
public abstract class TestSpringBase {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringBase.class);

    @Before
    public void initTestSpringBase() {
        logger.info("### start {}", this.getClass().getName());
    }
}
