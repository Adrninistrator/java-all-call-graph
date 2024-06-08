package test.runbycode;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBCRunnerGenAllGraph4Callee extends TestRunByCodeBase {
    @Test
    public void test() {
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());
    }
}
