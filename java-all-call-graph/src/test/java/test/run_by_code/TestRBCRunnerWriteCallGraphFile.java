package test.run_by_code;

import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import org.junit.Test;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
public class TestRBCRunnerWriteCallGraphFile extends TestRunByCodeBase {

    @Test
    public void test() {
        new RunnerWriteCallGraphFile().run(configureWrapper);
    }
}
