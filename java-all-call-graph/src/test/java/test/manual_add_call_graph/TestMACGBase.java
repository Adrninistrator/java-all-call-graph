package test.manual_add_call_graph;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import org.junit.Before;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

/**
 * @author adrninistrator
 * @date 2022/4/22
 * @description:
 */
public abstract class TestMACGBase {
    protected ConfigureWrapper configureWrapper = new ConfigureWrapper();

    @Before
    public void init() {
        configureWrapper.addConfig(ConfigKeyEnum.CKE_APP_NAME, "test_macg");
        /*
            执行以下命令生成：
            gradlew test_jar
         */
        configureWrapper.addConfig(ConfigKeyEnum.CKE_CALL_GRAPH_JAR_LIST, "build/libs/test.jar");
        configureWrapper.addConfig(ConfigKeyEnum.CKE_INPUT_IGNORE_OTHER_PACKAGE, Boolean.FALSE.toString());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_2.getDetail());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_THREAD_NUM, "20");
        configureWrapper.addConfig(ConfigKeyEnum.CKE_SHOW_METHOD_ANNOTATION, Boolean.TRUE.toString());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_GEN_COMBINED_OUTPUT, Boolean.TRUE.toString());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_SHOW_CALLER_LINE_NUM, Boolean.TRUE.toString());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.FALSE.toString());

        configureWrapper.addConfig(ConfigKeyEnum.CKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.addConfig(ConfigKeyEnum.CKE_DB_H2_FILE_PATH, "./build/jacg_h2db_macg");

        configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLEE_CLASS_NAME, new HashSet<>(Collections.singletonList("System")));
        configureWrapper.addOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD, new HashSet<>(Arrays.asList(
                "TestActionListener:test1",
                "TestActionListener:test2",
                "TestActionListener:test3",
                "TestFixedManualAddCallGraph:test1",
                "TestFixedManualAddCallGraph:test2",
                "TestUnfixedManualAddCallGraph:test1a",
                "TestUnfixedManualAddCallGraph:test1b",
                "TestUnfixedManualAddCallGraph:test2"
        )));
    }
}
