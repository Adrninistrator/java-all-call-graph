package test.runbycode.composite;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import org.junit.Test;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2024/3/14
 * @description:
 */

public class TestRBCFindStackComposite extends TestRunByCodeBase {
    public static final Boolean[] BOOLEAN_ARRAY = new Boolean[]{Boolean.FALSE, Boolean.TRUE};

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testCallee() {
        for (OutputDetailEnum outputDetailEnum : OutputDetailEnum.values()) {
            if (OutputDetailEnum.ODE_ILLEGAL == outputDetailEnum) {
                continue;
            }

            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, outputDetailEnum.getDetail());
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName
                    + JACGConstants.FLAG_AT + currentMethodName
                    + JACGConstants.FLAG_AT + outputDetailEnum.getDetail());
            boolean outputDetailIs0 = OutputDetailEnum.ODE_0 == outputDetailEnum;
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_STACK_OTHER_FORMS, String.valueOf(outputDetailIs0));

            FindCallStackTrace findCallStackTrace = new FindCallStackTrace(true, configureWrapper);
            runFindCallStackTraceAndCheck(findCallStackTrace);
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
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName
                        + JACGConstants.FLAG_AT + currentMethodName
                        + JACGConstants.FLAG_AT + outputDetailEnum.getDetail()
                        + JACGConstants.FLAG_AT + ignoreDup
                );
                boolean outputDetailIs0 = OutputDetailEnum.ODE_0 == outputDetailEnum;
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_STACK_OTHER_FORMS, String.valueOf(outputDetailIs0));

                FindCallStackTrace findCallStackTrace = new FindCallStackTrace(false, configureWrapper);
                runFindCallStackTraceAndCheck(findCallStackTrace);
            }
        }
    }
}
