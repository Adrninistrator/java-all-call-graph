package test.run_by_code.manual_add_method_call;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Before;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;
import test.call_graph.action_listener.TestActionListener;
import test.call_graph.manual_add_method_call.fixed.TestFixedManualAddMethodCall;
import test.call_graph.manual_add_method_call.unfixed.TestUnfixedManualAddMethodCall;
import test.run_by_code.base.TestRunByCodeBase;
import test.run_by_code.manual_add_method_call.extensions.MAMCExt4ActionListener;
import test.run_by_code.manual_add_method_call.extensions.MAMCExt4FixedService1;
import test.run_by_code.manual_add_method_call.extensions.MAMCExt4UnfixedService1;

/**
 * @author adrninistrator
 * @date 2022/4/22
 * @description:
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TestManualAddMethodCall extends TestRunByCodeBase {

    @Before
    public void initTestManualAddMethodCall() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_1.getDetail());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName()
        );
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestActionListener.class.getName(),
                TestFixedManualAddMethodCall.class.getName(),
                TestUnfixedManualAddMethodCall.class.getName()
        );
    }

    @Test
    public void test0WithOutExtensions() {
        // 指定扩展类为空
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1);

        new RunnerWriteDb().run(configureWrapper);

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Callee.class.getSimpleName());
        new RunnerGenAllGraph4Callee().run(configureWrapper);

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Caller.class.getSimpleName());
        new RunnerGenAllGraph4Caller().run(configureWrapper);
    }

    @Test
    public void test1WithExtensions() {
        // 指定扩展类
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1,
                MAMCExt4ActionListener.class.getName(),
                MAMCExt4FixedService1.class.getName(),
                MAMCExt4UnfixedService1.class.getName()
        );

        new RunnerWriteDb().run(configureWrapper);

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Callee.class.getSimpleName());
        new RunnerGenAllGraph4Callee().run(configureWrapper);

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Caller.class.getSimpleName());
        new RunnerGenAllGraph4Caller().run(configureWrapper);
    }
}
