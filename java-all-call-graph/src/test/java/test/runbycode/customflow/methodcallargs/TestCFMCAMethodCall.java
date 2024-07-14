package test.runbycode.customflow.methodcallargs;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import test.callgraph.customflow.methodcallargs.service.TestCFMCAService1;
import test.callgraph.customflow.methodcallargs.service.TestCFMCAService2;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.customflow.methodcallargs.handler.CFMCAMethodCallHandler;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
public class TestCFMCAMethodCall extends TestRunByCodeBase {

    @Before
    public void initTestCFMCAMethodCall() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_1.getDetail());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                System.class.getName()
        );
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                TestCFMCAService1.class.getName(),
                TestCFMCAService2.class.getName()
        );
    }

    @Test
    public void test0NoAddMethodCall() {
        commonWriteDb();
        // 不添加方法调用关系

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Callee.class.getSimpleName());
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Caller.class.getSimpleName());
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }

    @Test
    public void test1AddMethodCall() {
        commonWriteDb();
        // 添加方法调用关系，需要在写数据库之后执行
        try (CFMCAMethodCallHandler cfmcaMethodCallHandler = new CFMCAMethodCallHandler(configureWrapper)) {
            cfmcaMethodCallHandler.addMethodCall();
        }

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Callee.class.getSimpleName());
        Assert.assertTrue(new RunnerGenAllGraph4Callee(configureWrapper).run());

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Caller.class.getSimpleName());
        Assert.assertTrue(new RunnerGenAllGraph4Caller(configureWrapper).run());
    }
}
