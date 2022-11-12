package test.run_by_code;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import org.junit.Before;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public abstract class TestRunByCodeBase {
    protected ConfigureWrapper configureWrapper = new ConfigureWrapper();

    @Before
    public void initTestRunByCodeBase() {
        configureWrapper.addConfig(ConfigKeyEnum.CKE_APP_NAME, "test_rbc");
        configureWrapper.addConfig(ConfigKeyEnum.CKE_CALL_GRAPH_JAR_LIST, "build/libs/test.jar");
        configureWrapper.addConfig(ConfigKeyEnum.CKE_INPUT_IGNORE_OTHER_PACKAGE, Boolean.TRUE.toString());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_2.getDetail());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_THREAD_NUM, "20");
        configureWrapper.addConfig(ConfigKeyEnum.CKE_SHOW_METHOD_ANNOTATION, Boolean.TRUE.toString());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_GEN_COMBINED_OUTPUT, Boolean.TRUE.toString());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_SHOW_CALLER_LINE_NUM, Boolean.TRUE.toString());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.FALSE.toString());

        // H2
        configureWrapper.addConfig(ConfigKeyEnum.CKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");

        // MySQL
//        configureWrapper.addConfig(ConfigKeyEnum.CKE_DB_USE_H2, Boolean.FALSE.toString());
//        configureWrapper.addConfig(ConfigKeyEnum.CKE_DB_DRIVER_NAME, Driver.class.getName());
//        configureWrapper.addConfig(ConfigKeyEnum.CKE_DB_URL,
//                "jdbc:mysql://1.1.1.1:3306/test_db?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true");
//        configureWrapper.addConfig(ConfigKeyEnum.CKE_DB_USERNAME, "username");
//        configureWrapper.addConfig(ConfigKeyEnum.CKE_DB_PASSWORD, "password");

        configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IN_ALLOWED_CLASS_PREFIX, new HashSet<>(Arrays.asList(
                "test.call_graph",
                "java.")));
        configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLEE_CLASS_NAME, new HashSet<>(Arrays.asList(
                "TestMCCallee",
                "System",
                "MethodWithAnnotation")));
        configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD, new HashSet<>(Arrays.asList(
                "TestMCCaller:test1a(",
                "TestMCCaller:test1b(",
                "TestMCCaller:test1c(",
                "TestArgument1:test(",
                "TestArgument2:test(",
                "CallMethodWithAnnotation:test1(")));
        configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD_IGNORE_PREFIX, new HashSet<>(Arrays.asList(
                "test4i",
                "test5")));
        configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLER_IGNORE_CLASS_KEYWORD, new HashSet<>(Collections.singletonList("TestArgument1")));
        configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLER_IGNORE_FULL_METHOD_PREFIX, new HashSet<>(Collections.singletonList(
                "test.call_graph.argument.TestArgument2")));
        configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLER_IGNORE_METHOD_PREFIX, new HashSet<>(Collections.singletonList("test1")));

        configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE, Arrays.asList(
                "!entry!",
                "<init>"));
        configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLER, Arrays.asList(
                "System",
                "Deprecated"));
    }
}
