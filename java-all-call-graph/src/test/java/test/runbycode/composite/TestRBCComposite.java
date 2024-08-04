package test.runbycode.composite;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.jacg.handler.classes.ClassLineNumberHandler;
import com.adrninistrator.jacg.handler.extendsimpl.JACGExtendsImplHandler;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import org.junit.Test;
import test.callgraph.argument.TestArgument1;
import test.callgraph.argument.TestArgument2;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2022/11/13
 * @description:
 */
public class TestRBCComposite extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test1NoIgnore() {
//        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH,"D:/desktop");
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();

        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                FindCallStackTrace.class.getSimpleName() + JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLEE);
        runFindCallStackTraceAndCheck(new FindCallStackTrace(true, configureWrapperCopy));

        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                FindCallStackTrace.class.getSimpleName() + JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLER);
        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_JSON_CALLER, Boolean.TRUE.toString());
        runFindCallStackTraceAndCheck(new FindCallStackTrace(false, configureWrapperCopy));
    }

    @Test
    public void test2WithIgnore() {
        ConfigureWrapper configureWrapperCopy = configureWrapper.copy();
        configureWrapperCopy.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_CLASS_KEYWORD,
                TestArgument1.class.getSimpleName());
        configureWrapperCopy.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_FULL_METHOD_PREFIX,
                TestArgument2.class.getName());
        configureWrapperCopy.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IGNORE_METHOD_PREFIX,
                "test1(");

        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                FindCallStackTrace.class.getSimpleName() + JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLEE);
        runFindCallStackTraceAndCheck(new FindCallStackTrace(true, configureWrapperCopy));

        configureWrapperCopy.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_NAME, currentClassName + JACGConstants.FLAG_AT + currentMethodName + JACGConstants.FLAG_AT +
                FindCallStackTrace.class.getSimpleName() + JACGConstants.DIR_OUTPUT_GRAPH_FOR_CALLER);
        runFindCallStackTraceAndCheck(new FindCallStackTrace(false, configureWrapperCopy));
    }

    @Test
    public void testWriteDbHandler() {
        try (ClassInfoHandler classInfoHandler = new ClassInfoHandler(configureWrapper);
             MethodInfoHandler methodInfoHandler = new MethodInfoHandler(configureWrapper);
             MethodCallInfoHandler methodCallInfoHandler = new MethodCallInfoHandler(configureWrapper);
             ClassLineNumberHandler classLineNumberHandler = new ClassLineNumberHandler(configureWrapper);
             JACGExtendsImplHandler jacgExtendsImplHandler = new JACGExtendsImplHandler(configureWrapper);
             SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            commonWriteDb();
        }
    }
}
