package test.runbycode;

import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
public class TestRBCRunnerWriteCallGraphFile extends TestRunByCodeBase {

    @Test
    public void test() {
        Assert.assertTrue(new RunnerWriteCallGraphFile(configureWrapper).run());
    }
}
