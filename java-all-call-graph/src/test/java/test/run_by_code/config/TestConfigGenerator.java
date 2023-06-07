package test.run_by_code.config;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import test.call_graph.annotation.CallMethodWithAnnotation;
import test.call_graph.annotation.MethodWithAnnotation;
import test.call_graph.argument.TestArgument1;
import test.call_graph.argument.TestArgument2;
import test.call_graph.argument.TestArgumentGenerics1;
import test.call_graph.cycle_call.TestCycleCall1;
import test.call_graph.extend_complex.ChildClassA1;
import test.call_graph.extend_complex.ChildClassA2;
import test.call_graph.extend_complex.ChildClassB1;
import test.call_graph.extend_complex.ChildClassB2;
import test.call_graph.extend_complex.TestExtendComplex;
import test.call_graph.interfaces.interfaces.InterfaceSuper1;
import test.call_graph.interfaces.interfaces.InterfaceSuper2;
import test.call_graph.method_call.TestMCCallee;
import test.call_graph.method_call.TestMCCaller;
import test.call_graph.spring.bean.use.complex.TestUseComplexService;
import test.call_graph.spring.mvc.TestSpringController1;

/**
 * @author adrninistrator
 * @date 2023/4/28
 * @description:
 */
public class TestConfigGenerator {

    // 生成通用的参数配置
    public static ConfigureWrapper genConfigureWrapper() {
        // java-all-call-graph的配置
        ConfigureWrapper configureWrapper = new ConfigureWrapper();
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
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, com.mysql.cj.jdbc.Driver.class.getName());
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
//                "jdbc:mysql://x.x.x.x:3306/database?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true");
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "username");
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "password");

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
                TestMCCallee.class.getName() + ":20",
                TestMCCallee.class.getName() + ":run(",
                TestMCCallee.class.getName() + ":run(",
                System.class.getName(),
                MethodWithAnnotation.class.getName(),
                TestArgument1.class.getName(),
                TestCycleCall1.class.getName()
        );

        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                MethodWithAnnotation.class.getName(),
                TestMCCaller.class.getName() + ":20",
                TestArgument1.class.getName() + ":test(",
                TestArgument2.class.getName() + ":test(",
                TestArgument2.class.getName() + ":test(",
                TestArgumentGenerics1.class.getName(),
                CallMethodWithAnnotation.class.getName() + ":test1(",
                InterfaceSuper1.class.getName() + ":testSuper1(",
                InterfaceSuper2.class.getName() + ":testSuper2(",
                TestCycleCall1.class.getName(),
                ChildClassA1.class.getName(),
                ChildClassA2.class.getName(),
                ChildClassB1.class.getName(),
                ChildClassB2.class.getName(),
                TestExtendComplex.class.getName(),
                test.call_graph.future.CallableImpl.class.getName() + ":call(",
                TestSpringController1.class.getName(),
                TestUseComplexService.class.getName()
        );

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE,
                JACGConstants.CALLEE_FLAG_ENTRY_NO_TAB,
                JavaCGCommonNameConstants.METHOD_NAME_INIT
        );

        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                System.class.getName(),
                Deprecated.class.getName()
        );

        return configureWrapper;
    }
}
