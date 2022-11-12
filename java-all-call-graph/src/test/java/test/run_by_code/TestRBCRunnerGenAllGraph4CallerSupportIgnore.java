package test.run_by_code;

import com.adrninistrator.jacg.runner.RunnerGenAllGraph4CallerSupportIgnore;
import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBCRunnerGenAllGraph4CallerSupportIgnore extends TestRunByCodeBase {
    @Test
    public void test() {
        new RunnerGenAllGraph4CallerSupportIgnore().run(configureWrapper);
    }
}
