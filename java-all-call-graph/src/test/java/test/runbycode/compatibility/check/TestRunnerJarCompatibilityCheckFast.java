package test.runbycode.compatibility.check;

import com.adrninistrator.jacg.compatibility.runner.RunnerJarCompatibilityCheckFast;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/9/30
 * @description:
 */
public class TestRunnerJarCompatibilityCheckFast extends TestRunByCodeBase {

    @Test
    public void $test0WriteDbCheck() {
        RunnerJarCompatibilityCheckFast runnerJarCompatibilityCheckFast = new RunnerJarCompatibilityCheckFast(javaCG2ConfigureWrapper, configureWrapper);
        Assert.assertTrue(runnerJarCompatibilityCheckFast.run());
    }

    @Test
    public void testSkipWriteDbCheck() {
        RunnerJarCompatibilityCheckFast runnerJarCompatibilityCheckFast = new RunnerJarCompatibilityCheckFast(javaCG2ConfigureWrapper, configureWrapper);
        runnerJarCompatibilityCheckFast.setSkipWriteDb(true);
        Assert.assertTrue(runnerJarCompatibilityCheckFast.run());
    }
}
