package test.runbycode;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
@JACGExample(title = "获得方法向上到包含关键字的调用堆栈",
        desc = {"首先会生成指定方法向上的完整方法调用链"})
public class TestRBCFindCallStackTrace4ee extends TestRunByCodeBase {
    @Test
    public void test() {
        runFindCallStackTraceAndCheck(new FindCallStackTrace(true, configureWrapper));
    }

    @Test
    public void testAll() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setAllowAllClasses();
        runFindCallStackTraceAndCheck(new FindCallStackTrace(true, configureWrapperCopy));
    }
}
