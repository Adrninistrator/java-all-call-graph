package test.runbycode.config;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.javacg2.common.JavaCG2CommonNameConstants;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.annotation.CallMethodWithAnnotation;
import test.callgraph.annotation.MethodWithAnnotation;
import test.callgraph.branch.TestExceptions;
import test.callgraph.cyclecall.TestCycleCall1;
import test.callgraph.empty.TestEmptyClass1;
import test.callgraph.empty.TestNoMethodClass1;
import test.callgraph.extendcomplex.ChildClassA1;
import test.callgraph.extendcomplex.ChildClassA2;
import test.callgraph.extendcomplex.ChildClassB1;
import test.callgraph.extendcomplex.ChildClassB2;
import test.callgraph.extendcomplex.TestExtendComplex;
import test.callgraph.innerclass.TestUseInnerClass;
import test.callgraph.interfaces.interfaces.InterfaceSuper1;
import test.callgraph.interfaces.interfaces.InterfaceSuper2;
import test.callgraph.interfacesdefault.TestUseInterfaceDefault1;
import test.callgraph.interfacesgeneric.TestInterfacesGeneric1;
import test.callgraph.lambda.TestLambda;
import test.callgraph.manualaddmethodcall.fixed.TestFixedManualAddMethodCall;
import test.callgraph.manualaddmethodcall.unfixed.TestUnfixedManualAddMethodCall;
import test.callgraph.methodargument.TestArgument1;
import test.callgraph.methodargument.TestArgument2;
import test.callgraph.methodargument.TestArgumentGenerics1;
import test.callgraph.methodcall.TestMCCallee;
import test.callgraph.methodcall.TestMCCaller;
import test.callgraph.mybatis.service.impl.Service1BImpl;
import test.callgraph.mybatis.service.listener.ListenerService1Impl;
import test.callgraph.polymorphism.childoverride.TestUseChildOverride;
import test.callgraph.spring.bean.use.complex.TestUseComplexService;
import test.callgraph.spring.mvc.TestSpringController1;
import test.callgraph.spring.tx.TestUseSpringTx;

/**
 * @author adrninistrator
 * @date 2023/4/28
 * @description:
 */
public class TestConfigGenerator {

    private static final Logger logger = LoggerFactory.getLogger(TestConfigGenerator.class);

    public static final String H2_DB_PATH_PREFIX = "./build/jacg_h2db_rbc";

    public static final String RUN_BY_CODE_APP_NAME = "test_rbc";
    public static final String RUN_BY_CODE_APP_NAME_WITH_HEAD = "_" + RUN_BY_CODE_APP_NAME;

    public static final String TEST_NOT_EXISTS_CLASS_NAME = "test_not_exists_class";

    public static final String TEST_JAR_PATH = "build/test.jar";

    // 设置基本的配置参数
    public static void setBaseConfig(ConfigureWrapper configureWrapper) {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_APP_NAME, RUN_BY_CODE_APP_NAME);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_1.getDetail());
//        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_2.getDetail());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_THREAD_NUM, "20");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, Boolean.FALSE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, "1000");
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, "");
    }

    // 使用H2数据库
    public static void useH2Db(ConfigureWrapper configureWrapper) {
        doUseH2Db(configureWrapper, H2_DB_PATH_PREFIX + JACGConstants.H2_FILE_EXT);
    }

    // 使用H2数据库
    public static void useH2DbPrefix(ConfigureWrapper configureWrapper) {
        doUseH2Db(configureWrapper, H2_DB_PATH_PREFIX);
    }

    private static void doUseH2Db(ConfigureWrapper configureWrapper, String h2DbPath) {
        logger.info("使用H2数据库");
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, h2DbPath);
    }

    // 设置需要生成向上的方法完整调用链的类或方法
    public static void setGenCalleeGraphMethodClass(ConfigureWrapper configureWrapper) {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                TestMCCallee.class.getName() + ":1",
                TestMCCallee.class.getName() + ":22",
                TestMCCallee.class.getName() + ":27",
                TestMCCallee.class.getName() + ":run(",
                TestMCCallee.class.getName() + ":test",
                System.class.getName(),
                MethodWithAnnotation.class.getName(),
                TestArgument1.class.getName(),
                TestArgument2.class.getName() + ":testNoCaller(",
                TestArgument2.class.getName() + ":testNotExist(",
                TestCycleCall1.class.getName(),
                TestSpringController1.class.getName() + ":get(",
                TestEmptyClass1.class.getName(),
                TestNoMethodClass1.class.getName(),
                TEST_NOT_EXISTS_CLASS_NAME
        );
    }

    // 设置需要生成向下的方法完整调用链的类或方法
    public static void setGenCallerGraphMethodClass(ConfigureWrapper configureWrapper) {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER,
                MethodWithAnnotation.class.getName(),
                TestMCCaller.class.getName() + ":1",
                TestMCCaller.class.getName() + ":55",
                TestArgument1.class.getName() + ":test",
                TestArgument2.class.getName() + ":test",
                TestArgument2.class.getName() + ":test(",
                TestArgument2.class.getName() + ":test(",
                TestArgument2.class.getName() + ":testNoCallee(",
                TestArgument2.class.getName() + ":testNotExist(",
                TestArgumentGenerics1.class.getName(),
                CallMethodWithAnnotation.class.getName() + ":test1(",
                InterfaceSuper1.class.getName() + ":testSuper1(",
                InterfaceSuper2.class.getName() + ":testSuper2(",
                TestCycleCall1.class.getName(),
                ChildClassA1.class.getName(),
                ChildClassA2.class.getName(),
                ChildClassB1.class.getName(),
                ChildClassB2.class.getName(),
                TestExceptions.class.getName(),
                TestExtendComplex.class.getName(),
                test.callgraph.future.CallableImpl.class.getName() + ":call(",
                TestSpringController1.class.getName(),
                TestUseComplexService.class.getName(),
                TestLambda.class.getName(),
                TestInterfacesGeneric1.class.getName(),
                TestUseInterfaceDefault1.class.getName(),
                TestEmptyClass1.class.getName(),
                TestNoMethodClass1.class.getName(),
                TestFixedManualAddMethodCall.class.getName(),
                TestUnfixedManualAddMethodCall.class.getName(),
                TestUseChildOverride.class.getName(),
                TestUseSpringTx.class.getName(),
                TestUseInnerClass.class.getName(),
                Service1BImpl.class.getName(),
                ListenerService1Impl.class.getName(),
                TEST_NOT_EXISTS_CLASS_NAME
        );
    }

    // 设置生成方法调用堆栈文件时，生成其他形式的调用堆栈文件
    public static void setCallGraphGenStackOtherForms(ConfigureWrapper configureWrapper) {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_STACK_OTHER_FORMS, Boolean.TRUE.toString());
//        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_STACK_OTHER_FORMS, Boolean.FALSE.toString());
    }

    // 设置生成向上的方法完整调用链文件后，从最底层被调用方法开始向上查找包含指定关键字的方法的调用堆栈时，使用的关键字
    public static void setFindStackKeyword4ee(ConfigureWrapper configureWrapper) {
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE,
                JACGConstants.CALLEE_FLAG_ENTRY_NO_TAB,
                JavaCG2CommonNameConstants.METHOD_NAME_INIT
        );

        // 设置生成方法调用堆栈文件时，生成其他形式的调用堆栈文件
        setCallGraphGenStackOtherForms(configureWrapper);
    }

    // 设置生成向下的方法完整调用链文件后，从最顶层调用方法开始向下查找包含指定关键字的方法的调用堆栈时，使用的关键字
    public static void setFindStackKeyword4er(ConfigureWrapper configureWrapper) {
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                System.class.getSimpleName() + JavaCG2Constants.FLAG_COLON,
                Deprecated.class.getName()
        );

        // 设置生成方法调用堆栈文件时，生成其他形式的调用堆栈文件
        setCallGraphGenStackOtherForms(configureWrapper);
    }

    // 生成通用的参数配置
    public static ConfigureWrapper genConfigureWrapper() {
        // java-all-call-graph的配置
        ConfigureWrapper configureWrapper = new ConfigureWrapper();

        // 设置基本的配置参数
        setBaseConfig(configureWrapper);

        // 使用H2数据库
        useH2Db(configureWrapper);

        // 使用MySQL数据库
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.FALSE.toString());
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, com.mysql.cj.jdbc.Driver.class.getName());
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_URL,
//                "jdbc:mysql://x.x.x.x:3306/database?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true");
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_USERNAME, "username");
//        configureWrapper.setMainConfig(ConfigDbKeyEnum.CDKE_DB_PASSWORD, "password");

        // 设置需要生成向上的方法完整调用链的类或方法
        setGenCalleeGraphMethodClass(configureWrapper);

        // 设置需要生成向下的方法完整调用链的类或方法
        setGenCallerGraphMethodClass(configureWrapper);

        // 设置生成向上的方法完整调用链文件后，从最底层被调用方法开始向上查找包含指定关键字的方法的调用堆栈时，使用的关键字
        setFindStackKeyword4ee(configureWrapper);

        // 设置生成向下的方法完整调用链文件后，从最顶层调用方法开始向下查找包含指定关键字的方法的调用堆栈时，使用的关键字
        setFindStackKeyword4er(configureWrapper);

        return configureWrapper;
    }

    // 生成 java-callgraph2 使用的配置参数包装类
    public static JavaCG2ConfigureWrapper genJavaCG2ConfigureWrapper() {
        JavaCG2ConfigureWrapper javaCG2ConfigureWrapper = new JavaCG2ConfigureWrapper();
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_LOG_METHOD_SPEND_TIME, Boolean.TRUE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR, Boolean.FALSE.toString());
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_OUTPUT_FILE_EXT, JavaCG2Constants.EXT_MD);
        javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP, Boolean.TRUE.toString());

        /*
            test.jar通过执行以下命令生成：
            gradlew test_jar
         */
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                TEST_JAR_PATH);
        return javaCG2ConfigureWrapper;
    }
}
