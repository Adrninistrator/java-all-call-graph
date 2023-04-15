package test.run_by_code.base;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.call_graph.annotation.MethodWithAnnotation;
import test.call_graph.argument.TestArgument1;
import test.call_graph.cycle_call.TestCycleCall1;
import test.call_graph.extend_complex.ChildClassA1;
import test.call_graph.extend_complex.ChildClassA2;
import test.call_graph.extend_complex.ChildClassB1;
import test.call_graph.extend_complex.ChildClassB2;
import test.call_graph.extend_complex.TestExtendComplex;
import test.call_graph.spring.mvc.TestSpringController1;

import java.lang.reflect.Method;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public abstract class TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestRunByCodeBase.class);

    protected ConfigureWrapper configureWrapper;

    protected void initCommon() {
        // java-all-call-graph的配置
        configureWrapper = new ConfigureWrapper();
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, "test_rbc");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_2.getDetail());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_THREAD_NUM, "20");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, "1000");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, "");

        // H2
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, "./build/jacg_h2db_rbc");

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

        // 添加所有预置的扩展类
        configureWrapper.addAllPreBuildExtensions();

        try {
            Class<?> testRunLocalConfigClass = Class.forName("test.run_local.TestRunLocalConfig");
            Method testRunLocalConfigSetMethod = testRunLocalConfigClass.getMethod("setConfig", ConfigureWrapper.class);
            testRunLocalConfigSetMethod.invoke(testRunLocalConfigClass, configureWrapper);
            logger.warn("!!!调用本地的修改配置参数方法!!!");
        } catch (ClassNotFoundException e) {
            logger.info("未找到指定的类 {}", e.getMessage());
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    @Before
    public void initTestRunByCodeBase() {
        initCommon();

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                "TestMCCallee:20",
                "TestMCCallee:run(",
                "test.call_graph.method_call.TestMCCallee:run(",
                System.class.getName(),
                MethodWithAnnotation.class.getName(),
                TestArgument1.class.getName(),
                TestCycleCall1.class.getName()
        );
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                MethodWithAnnotation.class.getName(),
                "TestMCCaller:20",
                "TestArgument1:test(",
                "TestArgument2:test(",
                "test.call_graph.argument.TestArgument2:test(",
                "CallMethodWithAnnotation:test1(",
                "InterfaceSuper1:testSuper1(",
                "InterfaceSuper2:testSuper2(",
                TestCycleCall1.class.getName(),
                ChildClassA1.class.getName(),
                ChildClassA2.class.getName(),
                ChildClassB1.class.getName(),
                ChildClassB2.class.getName(),
                TestExtendComplex.class.getName(),
                "test.call_graph.future.CallableImpl:call(",
                TestSpringController1.class.getName()
        );

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE,
                JACGConstants.CALLEE_FLAG_ENTRY_NO_TAB,
                JavaCGCommonNameConstants.METHOD_NAME_INIT
        );

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                System.class.getName(),
                Deprecated.class.getName()
        );
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
