package test.custom_flow.method_call_args;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Test;
import test.run_by_code.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
public class TestCFMCACallGraph extends TestRunByCodeBase {

    @Test
    public void test() {
        if (!new RunnerWriteDb().run(configureWrapper)) {
            return;
        }


    }
}
