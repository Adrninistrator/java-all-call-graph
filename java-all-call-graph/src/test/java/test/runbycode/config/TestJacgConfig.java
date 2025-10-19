package test.runbycode.config;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/8/8
 * @description:
 */
public class TestJacgConfig extends TestRunByCodeBase {

    @Test
    public void test1WriteDb() {
        TestConfigGenerator.useH2Db(configureWrapper);
        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper);
        Assert.assertTrue(runnerWriteDb.run());
    }

    @Test
    public void test2GenCallGraph() {
        TestConfigGenerator.useH2DbPrefix(configureWrapper);
        RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runnerGenAllGraph4Callee.run());
    }
}
