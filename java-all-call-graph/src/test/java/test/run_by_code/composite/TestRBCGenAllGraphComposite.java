package test.run_by_code.composite;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.junit.Before;
import org.junit.Test;
import test.run_by_code.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description:
 */

public class TestRBCGenAllGraphComposite extends TestRunByCodeBase {
    public static final Boolean[] BOOLEAN_ARRAY = new Boolean[]{Boolean.FALSE, Boolean.TRUE};

    @Before
    public void initTestRunnerGenAllGraphComposite() {
        commonInsertDb();
    }

    @Test
    public void testCallee() {
        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            if (OutputDetailEnum.ODE_ILLEGAL == outputDetailEnum) {
                continue;
            }

            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, outputDetailEnum.getDetail());
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName
                    + JACGConstants.FLAG_AT + currentMethodName
                    + JACGConstants.FLAG_AT + outputDetailEnum.getDetail());

            RunnerGenAllGraph4Callee runnerGenAllGraph4Callee = new RunnerGenAllGraph4Callee();
            if (!runnerGenAllGraph4Callee.run(configureWrapper)) {
                throw new JavaCGRuntimeException("失败");
            }
        }
    }

    @Test
    public void testCaller() {
        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            if (OutputDetailEnum.ODE_ILLEGAL == outputDetailEnum) {
                continue;
            }

            for (boolean ignoreDup : BOOLEAN_ARRAY) {
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, outputDetailEnum.getDetail());
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, String.valueOf(ignoreDup));
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName
                        + JACGConstants.FLAG_AT + currentMethodName
                        + JACGConstants.FLAG_AT + outputDetailEnum.getDetail()
                        + JACGConstants.FLAG_AT + ignoreDup
                );

                RunnerGenAllGraph4Caller runnerGenAllGraph4Caller = new RunnerGenAllGraph4Caller();
                if (!runnerGenAllGraph4Caller.run(configureWrapper)) {
                    throw new JavaCGRuntimeException("失败");
                }
            }
        }
    }
}
