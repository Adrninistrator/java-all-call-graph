package test.run_by_code;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBC0RunnerWriteDb extends TestRunByCodeBase {
    @Test
    public void test() {
        RunnerWriteDb runnerWriteDb = new RunnerWriteDb();
        Assert.assertTrue(runnerWriteDb.run(configureWrapper, javaCGConfigureWrapper));
        Assert.assertFalse(runnerWriteDb.run(configureWrapper, javaCGConfigureWrapper));
    }
}
