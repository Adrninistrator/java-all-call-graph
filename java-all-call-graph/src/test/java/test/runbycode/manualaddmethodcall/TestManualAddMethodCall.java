package test.runbycode.manualaddmethodcall;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import test.callgraph.awt.TestActionListener;
import test.callgraph.manualaddmethodcall.fixed.TestFixedManualAddMethodCall;
import test.callgraph.manualaddmethodcall.unfixed.TestUnfixedManualAddMethodCall;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.manualaddmethodcall.extensions.MAMCExt4ActionListener;
import test.runbycode.manualaddmethodcall.extensions.MAMCExt4FixedService1;
import test.runbycode.manualaddmethodcall.extensions.MAMCExt4UnfixedService1;

/**
 * @author adrninistrator
 * @date 2022/4/22
 * @description:
 */
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

        commonWriteDb();

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Callee.class.getSimpleName());
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Caller.class.getSimpleName());
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @Test
    public void test1WithExtensions() {
        // 指定扩展类
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1,
                MAMCExt4ActionListener.class.getName(),
                MAMCExt4FixedService1.class.getName(),
                MAMCExt4UnfixedService1.class.getName()
        );

        commonWriteDb();

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Callee.class.getSimpleName());
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Caller.class.getSimpleName());
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }
}
