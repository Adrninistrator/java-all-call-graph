package test.run_by_code.multi_times;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description:
 */
public class TestRBCRunnerGenAllGraph4CallerMultiTimes extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestRBCRunnerGenAllGraph4CallerMultiTimes.class);

    @Test
    public void test() {
        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller();
        Assert.assertTrue(runnerGenAllGraph4Caller.run(configureWrapper));
        logger.error("以下出现ERROR日志是符合预期的，同一个类执行多次会出现以下提示");
        Assert.assertFalse(runnerGenAllGraph4Caller.run(configureWrapper));
    }
}
