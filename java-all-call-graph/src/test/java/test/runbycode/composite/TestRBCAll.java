package test.runbycode.composite;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Assert;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
public class TestRBCAll extends TestRunByCodeBase {

    @Test
    public void test() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        Assert.assertTrue(new RunnerWriteCallGraphFile(configureWrapperCopy).run());

        configureWrapperCopy = configureWrapper.copy();
        Assert.assertTrue(new RunnerWriteDb(configureWrapperCopy).run());

        configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + FindCallStackTrace.class.getSimpleName() +
                JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLEE);
        runFindCallStackTraceAndCheck(new FindCallStackTrace(true, configureWrapperCopy));

        configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + FindCallStackTrace.class.getSimpleName() +
                JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLER);
        runFindCallStackTraceAndCheck(new FindCallStackTrace(false, configureWrapperCopy));

        configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Callee.class.getSimpleName());
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapperCopy).run());

        configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Caller.class.getSimpleName());
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapperCopy).run());
    }
}
