package test.runbycode.concurrent;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/11/8
 * @description:
 */
public class TestRBCConcurrentRead extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test() {
        ConfigureWrapper configureWrapper1 = configureWrapper.copy();

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + FindCallStackTrace.class.getSimpleName() +
                JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLEE);
        Thread thread1 = new Thread(() -> {
            runFindCallStackTraceAndCheck(new FindCallStackTrace(true, configureWrapper));
        });
        thread1.start();

        configureWrapper1.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + FindCallStackTrace.class.getSimpleName() +
                JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLER);
        Thread thread2 = new Thread(() -> {
            runFindCallStackTraceAndCheck(new FindCallStackTrace(false, configureWrapper1));
        });
        thread2.start();

        try {
            thread1.join();
            thread2.join();
        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail();
        }
    }
}
