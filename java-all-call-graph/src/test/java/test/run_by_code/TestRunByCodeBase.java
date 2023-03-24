package test.run_by_code;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.conf.JavaCGConfigureWrapper;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public abstract class TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestRunByCodeBase.class);

    protected ConfigureWrapper configureWrapper = new ConfigureWrapper();
    protected JavaCGConfigureWrapper javaCGConfigureWrapper = JACGUtil.genJavaCGConfigureWrapper();

    @Before
    public void initTestRunByCodeBase() {
        // java-all-call-graph的配置
        // 清空全部参数，在设置参数之前执行，避免jar包或项目中的配置文件有值时对生成结果产生干扰
        configureWrapper.clearAllConfig();

        configureWrapper.setConfig(ConfigKeyEnum.CKE_APP_NAME, "test_rbc");
        configureWrapper.setConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_2.getDetail());
        configureWrapper.setConfig(ConfigKeyEnum.CKE_THREAD_NUM, "20");
        configureWrapper.setConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.FALSE.toString());
        configureWrapper.setConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, "1000");
        configureWrapper.setConfig(ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED, Boolean.TRUE.toString());

        // H2
        configureWrapper.setConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");

        // MySQL
//        configureWrapper.setConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
//        configureWrapper.setConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, com.mysql.cj.jdbc.Driver.class.getName());
//        configureWrapper.setConfig(ConfigDbKeyEnum.CDKE_DB_URL,
//                "jdbc:mysql://1.1.1.1:3306/test_db?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true");
//        configureWrapper.setConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "username");
//        configureWrapper.setConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "password");

        /*
            test.jar通过执行以下命令生成：
            gradlew test_jar
         */
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                "build/libs/test.jar"
        );

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX,
                "test.call_graph.",
                "java."
        );

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                "TestMCCallee:20",
                "TestMCCallee:run(",
                "test.call_graph.method_call.TestMCCallee:run(",
                "System",
                "MethodWithAnnotation",
                "TestArgument1",
                "TestCycleCall1"
        );
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                "MethodWithAnnotation",
                "TestMCCaller:20",
                "TestArgument1:test(",
                "TestArgument2:test(",
                "test.call_graph.argument.TestArgument2:test(",
                "CallMethodWithAnnotation:test1(",
                "InterfaceSuper1:testSuper1(",
                "InterfaceSuper2:testSuper2(",
                "TestCycleCall1",
                "ChildClassA1",
                "ChildClassA2",
                "ChildClassB1",
                "ChildClassB2",
                "TestExtendComplex",
                "test.call_graph.future.CallableImpl:call(",
                "TestSpringController1"
        );

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE,
                JACGConstants.CALLEE_FLAG_ENTRY_NO_TAB,
                JavaCGConstants.METHOD_NAME_INIT
        );

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                "System",
                "Deprecated"
        );

        // 添加所有预置的扩展类
        configureWrapper.addAllPreBuildExtensions();
    }

    protected void printListContent(List<?> objectList) {
        if (objectList == null) {
            return;
        }
        for (Object object : objectList) {
            logger.info("{}", JACGJsonUtil.getJsonStrPretty(object));
        }
    }
}
