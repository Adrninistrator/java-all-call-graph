package test.diffjar.task;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import test.diffjar.service.TestService1;

/**
 * @author adrninistrator
 * @date 2024/4/19
 * @description:
 */
public class TestTask1 {

    private static final Logger logger = LoggerFactory.getLogger(test.diffjar.task.TestTask1.class);

    @Autowired
    private TestService1 testService1;

    @Scheduled(cron = "*/1 * * * * *")
    public void test1() {
        String value = testService1.testA();
        logger.info("run {}", value);
    }

    @Scheduled(cron = "*/1 * * * * *")
    public void test2() {
        logger.info("run");
    }
}
