package test.runbycode;

import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class TestRBCFindCallStackTrace4er extends TestRunByCodeBase {
    @Test
    public void test() {
        runFindCallStackTraceAndCheck(new FindCallStackTrace(false, configureWrapper));
    }
}
