package test.runbycode.multitimes;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description:
 */
public class TestRBCRunnerGenAllGraph4CallerMultiTimes extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestRBCRunnerGenAllGraph4CallerMultiTimes.class);

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test1() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName);
        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Caller.run());
        logger.error("以下出现ERROR日志是符合预期的，同一个类执行多次会出现以下提示");
        Assert.assertFalse(runnerGenAllGraph4Caller.run());
    }

    @Test
    public void test2() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, "");
        RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Caller.run());
        logger.error("以下出现ERROR日志是符合预期的，同一个类执行多次会出现以下提示");
        Assert.assertFalse(runnerGenAllGraph4Caller.run());
    }
}
