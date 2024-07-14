package test.runbycode.multitimes;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
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
public class TestRBCRunnerGenAllGraph4CalleeMultiTimes extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestRBCRunnerGenAllGraph4CalleeMultiTimes.class);

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test1() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName);
        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Callee.run());
        logger.error("以下出现ERROR日志是符合预期的，同一个类执行多次会出现以下提示");
        Assert.assertFalse(runnerGenAllGraph4Callee.run());
    }

    @Test
    public void test2() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, "");
        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Callee.run());
        logger.error("以下出现ERROR日志是符合预期的，同一个类执行多次会出现以下提示");
        Assert.assertFalse(runnerGenAllGraph4Callee.run());
    }
}
