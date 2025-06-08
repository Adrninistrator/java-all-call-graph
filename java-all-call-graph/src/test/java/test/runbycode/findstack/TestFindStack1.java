package test.runbycode.findstack;

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

public class TestFindStack1 extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void testCallee() {
        OutputDetailEnum outputDetailEnum = OutputDetailEnum.ODE_0;
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, outputDetailEnum.getDetail());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName
                + JACGConstants.FLAG_AT + currentMethodName
                + JACGConstants.FLAG_AT + outputDetailEnum.getDetail());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_STACK_OTHER_FORMS, Boolean.TRUE.toString());

        FindCallStackTrace findCallStackTrace = new FindCallStackTrace(true, configureWrapper);
        runFindCallStackTraceAndCheck(findCallStackTrace);
    }

    @Test
    public void testCaller() {
        OutputDetailEnum outputDetailEnum = OutputDetailEnum.ODE_0;
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, outputDetailEnum.getDetail());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName
                + JACGConstants.FLAG_AT + currentMethodName
                + JACGConstants.FLAG_AT + outputDetailEnum.getDetail()
        );
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_STACK_OTHER_FORMS, Boolean.TRUE.toString());

        FindCallStackTrace findCallStackTrace = new FindCallStackTrace(false, configureWrapper);
        runFindCallStackTraceAndCheck(findCallStackTrace);
    }
}
