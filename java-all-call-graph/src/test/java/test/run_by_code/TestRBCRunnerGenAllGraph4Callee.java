package test.run_by_code;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
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
        new RunnerGenAllGraph4Callee().run(configureWrapper);
    }
}
