package test.run_by_code.handler.base;

import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Before;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description:
 */
public abstract class TestHandlerBase extends TestRunByCodeBase {

    private boolean inited = false;

    @Before
    public void initTestHandlerBase() {
        if (inited) {
            return;
        }
        new RunnerWriteDb().run(configureWrapper, javaCGConfigureWrapper);
        inited = true;
    }
}
