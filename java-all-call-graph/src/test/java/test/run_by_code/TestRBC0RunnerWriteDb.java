package test.run_by_code;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Test;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBC0RunnerWriteDb extends TestRunByCodeBase {
    @Test
    public void test() {
        new RunnerWriteDb().run(configureWrapper);
    }
}
