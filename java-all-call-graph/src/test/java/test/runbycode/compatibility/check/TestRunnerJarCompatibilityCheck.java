package test.runbycode.compatibility.check;

import com.adrninistrator.jacg.compatibility.runner.RunnerJarCompatibilityCheck;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/9/30
 * @description:
 */
public class TestRunnerJarCompatibilityCheck extends TestRunByCodeBase {

    @Test
    public void test() {
        commonWriteDb();

        RunnerJarCompatibilityCheck runnerJarCompatibilityCheck = new RunnerJarCompatibilityCheck(configureWrapper);
        Assert.assertTrue(runnerJarCompatibilityCheck.run());
    }
}
