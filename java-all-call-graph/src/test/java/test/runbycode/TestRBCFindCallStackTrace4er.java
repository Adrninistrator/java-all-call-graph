package test.runbycode;

import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
@JACGExample(title = "获得方法向下到包含关键字的调用堆栈",
        desc = {"首先会生成指定方法向下的完整方法调用链"})
public class TestRBCFindCallStackTrace4er extends TestRunByCodeBase {
    @Test
    public void test() {
        runFindCallStackTraceAndCheck(new FindCallStackTrace(false, configureWrapper));
    }
}
