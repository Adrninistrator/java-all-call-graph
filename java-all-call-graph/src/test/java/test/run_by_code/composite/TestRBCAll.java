package test.run_by_code.composite;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.find_stack.FindCallStackTrace;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Test;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2023/4/2
 * @description:
 */
public class TestRBCAll extends TestRunByCodeBase {

    @Test
    public void test() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        new RunnerWriteCallGraphFile().run(configureWrapperCopy);

        configureWrapperCopy = configureWrapper.copy();
        new RunnerWriteDb().run(configureWrapperCopy);

        configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + FindCallStackTrace.class.getSimpleName() +
                JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLEE);
        new FindCallStackTrace().find(true, configureWrapperCopy);

        configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + FindCallStackTrace.class.getSimpleName() +
                JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLER);
        new FindCallStackTrace().find(false, configureWrapperCopy);

        configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Callee.class.getSimpleName());
        new RunnerGenAllGraph4Callee().run(configureWrapperCopy);

        configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Caller.class.getSimpleName());
        new RunnerGenAllGraph4Caller().run(configureWrapperCopy);
    }
}
