package test.run_by_code;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import org.junit.Assert;
import org.junit.Test;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBCRunnerGenAllGraph4Callee extends TestRunByCodeBase {
    @Test
    public void test() {
        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee();
        Assert.assertTrue(runnerGenAllGraph4Callee.run(configureWrapper));
        Assert.assertFalse(runnerGenAllGraph4Callee.run(configureWrapper));
    }
}
