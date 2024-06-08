package test.callgraph.spring.bean.use.springtask;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * @author adrninistrator
 * @date 2024/3/24
 * @description:
 */
@Component
public class TestSpringTask {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringTask.class);

    @Scheduled(cron = "*/1 * * * * *")
    public void run1() {
        logger.info("run1");
    }
}
