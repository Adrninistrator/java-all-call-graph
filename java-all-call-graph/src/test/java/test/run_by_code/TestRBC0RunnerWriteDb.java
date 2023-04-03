package test.run_by_code;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBC0RunnerWriteDb extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestRBC0RunnerWriteDb.class);

    @Test
    public void test() {
        RunnerWriteDb runnerWriteDb = new RunnerWriteDb();
        Assert.assertTrue(runnerWriteDb.run(configureWrapper, javaCGConfigureWrapper));
        logger.error("以下出现ERROR日志是符合预期的，同一个类执行多次会出现以下提示");
        Assert.assertFalse(runnerWriteDb.run(configureWrapper, javaCGConfigureWrapper));
    }
}
