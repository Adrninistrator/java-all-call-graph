package test.run_by_code.custom_flow.method_call_args;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import org.junit.Before;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;
import test.call_graph.custom_flow.method_call_args.service.TestCFMCAService1;
import test.call_graph.custom_flow.method_call_args.service.TestCFMCAService2;
import test.run_by_code.base.TestRunByCodeBase;
import test.run_by_code.custom_flow.method_call_args.handler.CFMCAMethodCallHandler;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
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
        // 不添加方法调用关系
        new RunnerWriteDb().run(configureWrapper);

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Callee.class.getSimpleName());
        new RunnerGenAllGraph4Callee().run(configureWrapper);

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Caller.class.getSimpleName());
        new RunnerGenAllGraph4Caller().run(configureWrapper);
    }

    @Test
    public void test1AddMethodCall() {
        new RunnerWriteDb().run(configureWrapper);
        // 添加方法调用关系，需要在写数据库之后执行
        try (CFMCAMethodCallHandler cfmcaMethodCallHandler = new CFMCAMethodCallHandler(configureWrapper)) {
            cfmcaMethodCallHandler.addMethodCall();
        }

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Callee.class.getSimpleName());
        new RunnerGenAllGraph4Callee().run(configureWrapper);

        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_SUB_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                RunnerGenAllGraph4Caller.class.getSimpleName());
        new RunnerGenAllGraph4Caller().run(configureWrapper);
    }
}
