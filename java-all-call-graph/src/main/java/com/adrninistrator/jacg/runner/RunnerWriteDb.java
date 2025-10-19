package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.DbConfInfo;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.conf.writer.JACGConfigWriter;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSFormatedSql;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MybatisMSColumn;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.el.constants.ElConstants;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1;
import com.adrninistrator.jacg.extensions.methodcall.AbstractJACGMethodCallExtension;
import com.adrninistrator.jacg.handler.fieldrelationship.MethodCallPassedFieldRelationshipHandler;
import com.adrninistrator.jacg.handler.jarinfo.JarInfoHandler;
import com.adrninistrator.jacg.handler.mybatis.MyBatisMSJavaColumnHandler;
import com.adrninistrator.jacg.handler.writedb.AbstractWriteDbHandler;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassAnnotation;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassExtImplGenericsType;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassName;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassReference;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassSignatureGenericsType;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4DupClassInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4DupClassReference;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4DupFieldInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4DupMethodInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4EnumInitArgField;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4EnumInitAssignInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ExtendsImpl;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ExtendsImplPre;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4FieldAnnotation;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4FieldGenericsType;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4FieldInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4FieldRelationship;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4FieldUsageOther;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4GetMethod;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4InnerClassInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4JACGConfig;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4JarInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4JavaCG2Config;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4LambdaMethodInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodAnnotation;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodArgAnnotation;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodArgGenericsType;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodArgument;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCall;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCallInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCallMethodCallReturn;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCallNonStaticField;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCallRawCallee;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCallStaticField;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCallStaticFieldMCR;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCatch;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodFinally;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodLineNumber;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodReturnArgSeq;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodReturnCallId;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodReturnConstValue;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodReturnFieldInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodReturnGenericsType;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodThrow;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSFormatedSql;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSSelectColumn;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSSetColumn;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSTable;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSWhereColumn;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSWriteTable;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MybatisMSColumn;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MybatisMSEntity;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MybatisMSGetSetDb;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4PackageInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ParsedCustomData;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4PropertiesConf;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SetMethod;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SetMethodAssignInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SfFieldMethodCall;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringAopAdviceAround;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringAopAdviceJava;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringAopAdviceXml;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringAopAspectJava;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringAopAspectXml;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringAopPointcutJava;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringAopPointcutXml;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringBean;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringController;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringScanPackageJava;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringScanPackageXml;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringTaskJava;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringTaskXml;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.dto.output.JavaCG2OutputInfo;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import com.adrninistrator.javacg2.util.RunProcessUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description: 生成Java方法调用关系文件并写入数据库
 */

public class RunnerWriteDb extends RunnerWriteCallGraphFile {
    private static final Logger logger = LoggerFactory.getLogger(RunnerWriteDb.class);

    // 写数据库的结果信息
    protected final WriteDbResult writeDbResult = new WriteDbResult();

    // 人工添加方法调用关系类列表
    private List<AbstractManualAddMethodCall1> manualAddMethodCall1List;

    // 方法调用处理扩展类列表
    private List<AbstractJACGMethodCallExtension> jacgMethodCallExtensionList;

    // 是否使用H2数据库
    private boolean useH2Db;

    // 写数据库次数
    private int writeDbTimes = 0;

    // 跳过调用java-callgraph2的步骤
    private boolean skipCallJavaCG2 = false;

    // 是否需要解析Spring AOP信息
    private boolean parseSpringAopInfo;

    /**
     * 构造函数，使用配置文件中的参数
     */
    public RunnerWriteDb() {
        super();
    }

    /**
     * 构造函数，使用代码中指定的参数
     *
     * @param javaCG2ConfigureWrapper
     * @param configureWrapper
     */
    public RunnerWriteDb(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper) {
        super(javaCG2ConfigureWrapper, configureWrapper);
    }

    @Override
    protected boolean preHandle() {
        // 读取其他配置文件
        if (!useNeo4j()) {
            return initDb();
        }
        return true;
    }

    @Override
    protected void handle() {
        // 执行实际处理
        if (!operate()) {
            // 记录执行失败
            recordTaskFail();
        }
    }

    @Override
    protected boolean checkH2DbFile() {
        // 检查H2数据库文件是否可写，允许文件不存在
        return checkH2DbFileWritable(true);
    }

    private boolean initDb() {
        DbConfInfo dbConfInfo = dbOperator.getDbConfInfo();
        // 是否使用H2数据库
        useH2Db = dbConfInfo.isUseH2Db();
        if (!useH2Db && JACGSqlUtil.isMySQLDb(dbConfInfo.getDriverClassName())) {
            if (!dbConfInfo.getDbUrl().contains(JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS)) {
                logger.error("使用MySQL时，需要在参数指定标志以开启批量插入 {} {}", JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS, configureWrapper.genConfigUsage(ConfigDbKeyEnum.CDKE_DB_URL));
                return false;
            }
        }
        return true;
    }

    // 获得需要处理的jar文件列表
    private List<String> getJarPathList() {
        List<String> jarPathList = javaCG2ConfigureWrapper.getOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR);
        logger.info("{} 需要处理的jar文件或目录 {}", currentSimpleClassName, StringUtils.join(jarPathList, " "));
        return jarPathList;
    }

    // 执行实际处理
    private boolean operate() {
        boolean skipWhenNotModified = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_SKIP_WRITE_DB_WHEN_JAR_NOT_MODIFIED);
        // 判断需要解析的jar文件没有变化时是否跳过写数据库操作
        if (skipWhenNotModified) {
            // 获得需要处理的jar文件列表
            List<String> jarPathList = getJarPathList();
            /*
                检查配置文件中指定的jar文件是否都在jar_info表中且未发生变化（用于判断是否可以跳过写数据库步骤）
                检查允许处理的类名或包名前缀是否有变化
             */
            if (checkAllJarExistsNotModified(jarPathList)) {
                logger.warn("有通过参数指定，且jar文件没有变化，跳过写数据库操作");
                return true;
            }
        }

        logger.info("后续会尝试执行写数据库操作");
        writeDbTimes++;

        boolean dropOrTruncate = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_DROP_OR_TRUNCATE_TABLE);
        logger.info("写数据库表之前执行 {} 操作", (dropOrTruncate ? "DROP" : "TRUNCATE"));

        if (!useNeo4j()) {
            // 创建数据库表
            if (!createTables(dropOrTruncate)) {
                return false;
            }
        }

        // 清理数据库表
        if (!dropOrTruncate && !truncateTables()) {
            return false;
        }

        if (!useNeo4j()) {
            // 添加用于人工添加方法调用关系的处理类
            if (!addManualAddMethodCallExtensions()) {
                return false;
            }

            // 添加 java-all-call-graph 组件处理方法调用的扩展类
            if (!addJACGMethodCallExtensions()) {
                return false;
            }
        }

        parseSpringAopInfo = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_PARSE_SPRING_AOP_INFO);
        logger.info("{}需要解析Spring AOP信息", parseSpringAopInfo ? "" : "不");

        if (skipCallJavaCG2) {
            if (parseSpringAopInfo) {
                logger.error("需要解析Spring AOP信息时，不允许配置不调用java-callgraph2生成jar文件的方法调用关系");
                return false;
            }
            logger.info("已配置不调用java-callgraph2生成jar文件的方法调用关系");
        } else {
            if (parseSpringAopInfo) {
                /*
                    需要解析Spring AOP信息时
                    - 配置需要生成fat jar，以获得advice影响方法
                    - 处理方法调用时需要解析被调用对象和参数可能的类型与值，以获得Spring Bean信息
                 */
                javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_MERGE_SEPARATE_FAT_JAR, Boolean.TRUE.toString());
                javaCG2ConfigureWrapper.setMainConfig(JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.TRUE.toString());
            }
            // 调用java-callgraph2生成jar文件的方法调用关系
            if (!callJavaCallGraph2()) {
                return false;
            }
        }

        // 创建线程池，参数固定指定为10，即使用10个线程
        createThreadPoolExecutor(10);

        // 处理组件使用的配置参数
        if (!useNeo4j() && !handleConfig()) {
            return false;
        }

        // 处理引用的类信息，需要先处理
        if (!handleClassName()) {
            return false;
        }

        Set<String> withArgsGenericsTypeMethodHash = new HashSet<>();
        Set<String> withReturnGenericsTypeMethodHash = new HashSet<>();
        Set<Integer> withInfoCallIdSet = new HashSet<>();
        // 处理方法
        if (!handleMethod(withArgsGenericsTypeMethodHash, withReturnGenericsTypeMethodHash, withInfoCallIdSet)) {
            return false;
        }

        // 以下处理注解信息时会处理 Spring Task、Controller ，需要等前面的方法相关信息处理完毕
        wait4TPEDone();

        JavaCG2Counter springTaskAnnotationCounter = new JavaCG2Counter();
        // 处理Spring相关信息（不包括Spring Controller）
        if (!handleSpringInfo(springTaskAnnotationCounter)) {
            return false;
        }

        Set<String> withAnnotationMethodHashSet = new HashSet<>();
        // 处理注解信息
        if (!handleAnnotations(withAnnotationMethodHashSet, springTaskAnnotationCounter)) {
            return false;
        }

        // 枚举唯一类名集合
        Set<String> enumSimpleClassNameSet = new HashSet<>();
        // 处理类的信息
        if (!handleClassInfo(enumSimpleClassNameSet)) {
            return false;
        }

        /*
            涉及继承的唯一类名
            key     子类唯一类名
            value   对应的父类唯一类名
         */
        Map<String, String> extendsSimpleClassNameMap = new HashMap<>();

        // 处理继承与实现相关信息
        if (!handleExtendsImpl(extendsSimpleClassNameMap)) {
            return false;
        }

        // 处理Lambda表达式方法信息
        if (!handleLambdaMethodInfo()) {
            return false;
        }

        // 处理字段
        if (!handleField()) {
            return false;
        }

        Set<String> myBatisMapperMethodSet = new HashSet<>();
        Set<String> myBatisMapperMethodWriteSet = new HashSet<>();
        // 处理MyBatis信息
        if (!handleMyBatisInfo(myBatisMapperMethodSet, myBatisMapperMethodWriteSet)) {
            return false;
        }

        // 处理类的泛型相关信息
        if (!handleClassGenericsType()) {
            return false;
        }

        /*
            get方法对应的信息
            key
                唯一类名
            value
                get方法名Set
         */
        Map<String, Set<String>> getMethodSimpleClassMap = new HashMap<>();

        /*
            set方法对应的信息
            key
                唯一类名
            value
                set方法名Set
         */
        Map<String, Set<String>> setMethodSimpleClassMap = new HashMap<>();
        // 处理get/set方法，以对应的调用关系
        if (!handleFieldRelationship(extendsSimpleClassNameMap, enumSimpleClassNameSet, getMethodSimpleClassMap, setMethodSimpleClassMap)) {
            return false;
        }

        // 处理配置文件
        if (!handleConfFile()) {
            return false;
        }

        // 处理解析jar文件时获取的自定义数据
        if (!handleParsedCustomData()) {
            return false;
        }

        // 等待前面的其他文件写入数据库完毕
        wait4TPEDone();

        // 处理方法调用关系文件
        if (!handleMethodCall(extendsSimpleClassNameMap, withAnnotationMethodHashSet, withInfoCallIdSet, withArgsGenericsTypeMethodHash, withReturnGenericsTypeMethodHash,
                myBatisMapperMethodSet, myBatisMapperMethodWriteSet, getMethodSimpleClassMap, setMethodSimpleClassMap)) {
            return false;
        }

        // 等待写入方法调用数据完成
        wait4TPEDone();

        // 处理通过方法调用传递的get/set方法关联关系（需要在处理方法调用关系后面执行）
        if (!handleMethodCallPassedGetSet()) {
            return false;
        }

        // 等待通过方法调用传递的get/set方法关联关系处理完毕
        wait4TPEDone();

        // 处理MyBatis XML文件中sql脚本的字段与Java代码的关联关系（需要在处理方法调用关系后面执行）
        if (!handleMyBatisMSJavaColumn()) {
            return false;
        }

        // 等待写入数据完成
        wait4TPEDone();

        // 人工添加方法调用关系（需要在方法调用关系文件处理完毕后执行）
        if (!manualAddMethodCall()) {
            return false;
        }

        // 处理jar文件信息，在所有其他表写完后再写入，这样可以通过jar文件信息判断其他表有没有写入成功
        if (!handleJarInfo()) {
            return false;
        }

        // 等待所有写入数据完成
        wait4TPEDone();

        // 处理Spring AOP advice
        if (!handleSpringAopAdvice()) {
            return false;
        }

        // 检查执行结果
        if (!checkResult()) {
            return false;
        }

        if (useH2Db) {
            // 显示H2数据库JDBC URL
            printH2JdbcUrl();
        }

        // 处理Spring AOP advice影响的方法
        return handleSpringAopAdviceAffectedMethod();
    }

    // 处理Spring AOP advice影响的方法
    private boolean handleSpringAopAdviceAffectedMethod() {
        if (useNeo4j() || !parseSpringAopInfo) {
            return true;
        }

        // 将处理Spring AOP advice影响的方法需要的配置文件写到对应目录
        String springAopConfigDirPath = currentOutputDirPath + JACGConstants.DIR_SPRING_AOP_CONFIG;
        logger.info("处理Spring AOP advice影响的方法，保存配置文件的目录 {}", springAopConfigDirPath);
        JACGConfigWriter jacgConfigWriter = new JACGConfigWriter(springAopConfigDirPath);
        jacgConfigWriter.setBaseConfigureWrapper(configureWrapper);
        boolean result1 = jacgConfigWriter.genMainConfig(ConfigKeyEnum.values());
        boolean result2 = jacgConfigWriter.genMainConfig(ConfigDbKeyEnum.values());
        boolean result3 = jacgConfigWriter.genOtherConfig(OtherConfigFileUseListEnum.values());
        boolean result4 = jacgConfigWriter.genOtherConfig(OtherConfigFileUseSetEnum.values());
        boolean result5 = jacgConfigWriter.genElConfig(ElConfigEnum.values(), ElConstants.getElDirUsageMap());
        if (!result1 || !result2 || !result3 || !result4 || !result5) {
            return false;
        }

        // 启动新的进程处理Spring AOP advice影响的方法
        return runSpringAopAdviceAffectedMethod(springAopConfigDirPath);
    }

    // 启动新的进程处理Spring AOP advice影响的方法
    private boolean runSpringAopAdviceAffectedMethod(String springAopConfigDirPath) {
        JarInfoHandler jarInfoHandler = new JarInfoHandler(dbOperWrapper);
        WriteDbData4JarInfo fatJarInfo = jarInfoHandler.queryFatJarInfo();
        if (fatJarInfo == null) {
            logger.error("启动新的进程处理Spring AOP advice影响的方法时，未查询到fat jar信息");
            return false;
        }
        logger.info("fat jar文件路径 {}", fatJarInfo.getJarFullPath());

        String javaHome = System.getProperty("java.home");
        logger.info("当前使用的 javaHome {}", javaHome);

        // 获取classpath
        String classpath = System.getProperty("java.class.path");
        logger.info("当前使用的 classpath {}", classpath);

        String javaPath;
        String newClassPath;
        if (JavaCG2Util.checkWindowsOs()) {
            // Windows操作系统
            javaPath = javaHome + "\\bin\\java.exe";
            newClassPath = classpath + ";" + fatJarInfo.getJarFullPath();
        } else {
            javaPath = javaHome + "/bin/java";
            newClassPath = classpath + JavaCG2Constants.FLAG_COLON + fatJarInfo.getJarFullPath();
        }

        // 启动进程前，关闭当前使用的数据源
        dbOperator.closeDs(this);

        String[] args = new String[]{
                RunProcessUtil.handleProcessArg(javaPath),
                "-classpath", RunProcessUtil.handleProcessArg(newClassPath),
                "-D" + JavaCG2Constants.JVM_PROP_KEY_INPUT_ROOT_PATH + "=" + RunProcessUtil.handleProcessArg(springAopConfigDirPath),
                "-D" + JACGConstants.JVM_PROP_KEY_LOG_FILE_SUFFIX + "=" + JACGConstants.DIR_SPRING_AOP_CONFIG,
                "-Dfile.encoding=UTF-8",
                RunnerWriteSpringAopAdviceAffectedMethod.class.getName()
        };
        // 启动进程
        return RunProcessUtil.runProcess(args);
    }

    // 添加用于根据方法调用信息添加方法调用关系的处理类
    private boolean addManualAddMethodCallExtensions() {
        List<String> manualAddMethodCallClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1);
        if (JavaCG2Util.isCollectionEmpty(manualAddMethodCallClassList)) {
            logger.info("未指定用于人工添加方法调用关系的处理类，跳过 {}", configureWrapper.genConfigUsage(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1));
            manualAddMethodCall1List = Collections.emptyList();
            return true;
        }

        logger.info("指定用于人工添加方法调用关系的处理类\n{}", StringUtils.join(manualAddMethodCallClassList, "\n"));
        manualAddMethodCall1List = new ArrayList<>(manualAddMethodCallClassList.size());

        for (String manualAddMethodCallClassName : manualAddMethodCallClassList) {
            try {
                AbstractManualAddMethodCall1 manualAddMethodCall1 = JACGClassMethodUtil.genClassObject(manualAddMethodCallClassName, AbstractManualAddMethodCall1.class,
                        new Class<?>[]{DbOperWrapper.class}, new Object[]{dbOperWrapper});
                if (manualAddMethodCall1 == null) {
                    return false;
                }
                manualAddMethodCall1List.add(manualAddMethodCall1);
            } catch (Exception e) {
                logger.error("处理人工添加方法调用关系的处理类异常 {} ", manualAddMethodCallClassName, e);
                return false;
            }
        }
        return true;
    }

    // 添加 java-all-call-graph 组件处理方法调用的扩展类
    private boolean addJACGMethodCallExtensions() {
        List<String> jacgMethodCallExtensionClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL);
        if (JavaCG2Util.isCollectionEmpty(jacgMethodCallExtensionClassList)) {
            logger.info("未指定 java-all-call-graph 组件处理方法调用的扩展类，跳过 {}", configureWrapper.genConfigUsage(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_JACG_METHOD_CALL));
            jacgMethodCallExtensionList = Collections.emptyList();
            return true;
        }

        logger.info("指定 java-all-call-graph 组件处理方法调用的扩展类\n{}", StringUtils.join(jacgMethodCallExtensionClassList, "\n"));
        jacgMethodCallExtensionList = new ArrayList<>(jacgMethodCallExtensionClassList.size());

        for (String jacgMethodCallExtensionClassName : jacgMethodCallExtensionClassList) {
            try {
                AbstractJACGMethodCallExtension jacgMethodCallExtension = JACGClassMethodUtil.genClassObject(jacgMethodCallExtensionClassName,
                        AbstractJACGMethodCallExtension.class,
                        new Class<?>[]{DbOperWrapper.class}, new Object[]{dbOperWrapper});
                if (jacgMethodCallExtension == null) {
                    return false;
                }
                jacgMethodCallExtensionList.add(jacgMethodCallExtension);
            } catch (Exception e) {
                logger.error("处理 java-all-call-graph 组件处理方法调用的扩展类异常 {} ", jacgMethodCallExtensionClassName, e);
                return false;
            }
        }
        return true;
    }

    /**
     * 检查配置文件中指定的jar文件是否都在jar_info表中且未发生变化（用于判断是否可以跳过写数据库步骤）
     *
     * @param jarPathList
     * @return true: jar文件未发生变化 false: jar文件有发生变化
     */
    private boolean checkAllJarExistsNotModified(List<String> jarPathList) {
        // 使用H2数据库时，检查数据库文件
        if (useH2Db) {
            File h2DbFile = getH2DbFile();
            if (!h2DbFile.exists()) {
                logger.info("H2数据库文件不存在，不能跳过写数据库步骤 {}", h2DbFile.getAbsolutePath());
                return false;
            }
        }

        if (!dbOperator.checkTableExists(DbTableInfoEnum.DTIE_JAR_INFO)) {
            logger.info("数据库表不存在，不能跳过写数据库步骤 {}", DbTableInfoEnum.DTIE_JAR_INFO.getTableNameKeyword());
            return false;
        }

        // 检查配置文件与jar_info表中的jar文件是否不一致或出现变化
        return !checkSomeJarModified(jarPathList);
    }

    // 创建数据库表
    public boolean createTables(boolean dropOrTruncate) {
        logger.info("创建数据库表");
        for (DbTableInfoEnum dbTableInfoEnum : DbTableInfoEnum.values()) {
            if (ArrayUtils.isEmpty(dbTableInfoEnum.getColumns())) {
                continue;
            }
            if (dropOrTruncate) {
                if (!dbOperator.dropTable(dbTableInfoEnum.getTableName())) {
                    return false;
                }
            }

            // 读取建表sql语句
            String sql = readCreateTableSql(dbTableInfoEnum.getTableFileName());
            if (StringUtils.isBlank(sql)) {
                logger.error("创建数据库表的sql语句为空 {}", dbTableInfoEnum.getTableFileName());
                return false;
            }

            if (!dbOperator.createTable(sql)) {
                return false;
            }
        }

        return true;
    }

    // 读取建表sql语句
    private String readCreateTableSql(String sqlFileName) {
        String sqlFilePath = JavaCG2Util.getInputRootPath() + InputDirEnum.IDE_SQL.getDirName() + "/" + sqlFileName;
        List<String> sqlList = JavaCG2FileUtil.readFile2List(sqlFilePath);
        if (JavaCG2Util.isCollectionEmpty(sqlList)) {
            logger.error("文件内容为空 {}", sqlFilePath);
            return null;
        }

        StringBuilder stringBuilder = new StringBuilder();
        for (String sql : sqlList) {
            if (stringBuilder.length() > 0) {
                stringBuilder.append(JavaCG2Constants.NEW_LINE);
            }
            // 对建表sql语句进行转换
            stringBuilder.append(transformCreateTableSql(sql, useH2Db));
        }
        String createTableSql = stringBuilder.toString();
        logger.debug("建表sql: {}", createTableSql);
        return createTableSql;
    }

    // 对建表sql语句进行转换
    private String transformCreateTableSql(String sql, boolean useH2Db) {
        if (StringUtils.startsWithIgnoreCase(sql, JACGConstants.SQL_CREATE_TABLE_HEAD)) {
            // CREATE TABLE if not exists开头
            return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
        }

        String trimSql = sql.trim();
        if (StringUtils.startsWithAny(trimSql, "PRIMARY KEY", "INDEX", "UNIQUE INDEX")) {
            // PRIMARY KEY、INDEX、UNIQUE INDEX开头
            if (useH2Db) {
                return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix)
                        .replaceAll("\\([0-9]+\\)", "");
            }
            return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
        }

        // 其他情况
        return sql;
    }

    // 清理数据库表
    protected boolean truncateTables() {
        logger.info("清理数据库表");
        for (DbTableInfoEnum dbTableInfoEnum : DbTableInfoEnum.values()) {
            if (ArrayUtils.isNotEmpty(dbTableInfoEnum.getColumns()) &&
                    !dbOperator.truncateTable(dbTableInfoEnum.getTableName())) {
                return false;
            }
        }
        return true;
    }

    // 初始化用于写入数据库的类
    protected void initWriteDbHandler(AbstractWriteDbHandler<?> writeDbHandler) {
        if (!useNeo4j()) {
            writeDbHandler.setDbOperator(dbOperator);
        }
        writeDbHandler.setDbOperWrapper(dbOperWrapper);
        writeDbHandler.setDbInsertBatchSize(dbOperWrapper.getDbInsertBatchSize());
        writeDbHandler.setAppName(appName);
        writeDbHandler.setThreadPoolExecutor(threadPoolExecutor);
        writeDbHandler.setRunningTaskNum(runningTaskNum);
        writeDbHandler.setTaskQueueMaxSize(taskQueueMaxSize);
        writeDbHandler.init(writeDbResult);
    }

    protected WriteDbHandler4ClassName genWriteDbHandler4ClassName() {
        return new WriteDbHandler4ClassName(writeDbResult);
    }

    protected WriteDbHandler4ClassReference genWriteDbHandler4ClassReference() {
        return new WriteDbHandler4ClassReference(writeDbResult);
    }

    // 处理组件使用的配置参数
    private boolean handleConfig() {
        // 处理java-callgraph2组件使用的配置参数
        WriteDbHandler4JavaCG2Config writeDbHandler4JavaCG2Config = new WriteDbHandler4JavaCG2Config(writeDbResult);
        initWriteDbHandler(writeDbHandler4JavaCG2Config);
        if (!writeDbHandler4JavaCG2Config.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理java-all-call-graph组件使用的配置参数
        WriteDbHandler4JACGConfig writeDbHandler4JACGConfig = new WriteDbHandler4JACGConfig(writeDbResult);
        initWriteDbHandler(writeDbHandler4JACGConfig);
        return writeDbHandler4JACGConfig.handle(javaCG2OutputInfo);
    }

    // 处理引用的类信息，需要先处理
    private boolean handleClassName() {
        WriteDbHandler4ClassName writeDbHandler4ClassName = genWriteDbHandler4ClassName();
        initWriteDbHandler(writeDbHandler4ClassName);

        WriteDbHandler4ClassReference writeDbHandler4ClassReference = genWriteDbHandler4ClassReference();
        initWriteDbHandler(writeDbHandler4ClassReference);
        writeDbHandler4ClassReference.setWriteDbHandler4ClassName(writeDbHandler4ClassName);
        if (!writeDbHandler4ClassReference.handle(javaCG2OutputInfo)) {
            return false;
        }

        if (!useNeo4j()) {
            WriteDbHandler4DupClassReference writeDbHandler4DupClassReference = new WriteDbHandler4DupClassReference(writeDbResult);
            initWriteDbHandler(writeDbHandler4DupClassReference);
            writeDbHandler4DupClassReference.setWriteDbHandler4ClassName(writeDbHandler4ClassName);
            if (!writeDbHandler4DupClassReference.handle(javaCG2OutputInfo)) {
                return false;
            }
        }

        // 等待直到任务执行完毕，等待引用的类信息写入完毕，后面需要使用
        wait4TPEDone();

        if (writeDbHandler4ClassName.checkFailed()) {
            return false;
        }
        // 将类名表中的同名类更新为使用完整类名，并记录同名类
        return dbOperWrapper.updateSimpleClassName2Full();
    }

    // 处理jar文件信息
    private boolean handleJarInfo() {
        if (useNeo4j()) {
            return true;
        }
        WriteDbHandler4JarInfo writeDbHandler4JarInfo = new WriteDbHandler4JarInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4JarInfo);
        return writeDbHandler4JarInfo.handle(javaCG2OutputInfo);
    }

    // 处理注解信息
    private boolean handleAnnotations(Set<String> withAnnotationMethodHashSet, JavaCG2Counter springTaskAnnotationCounter) {
        if (useNeo4j()) {
            return true;
        }
        WriteDbHandler4MethodAnnotation writeDbHandler4MethodAnnotation = new WriteDbHandler4MethodAnnotation(writeDbResult);
        // 处理类注解信息
        WriteDbHandler4ClassAnnotation writeDbHandler4ClassAnnotation = new WriteDbHandler4ClassAnnotation(writeDbResult);
        initWriteDbHandler(writeDbHandler4ClassAnnotation);
        if (!writeDbHandler4ClassAnnotation.handle(javaCG2OutputInfo)) {
            return false;
        }

        Map<String, List<String>> classRequestMappingMap = writeDbHandler4ClassAnnotation.getClassRequestMappingMap();

        // 创建处理Spring Controller信息的类
        WriteDbHandler4SpringController writeDbHandler4SpringController = new WriteDbHandler4SpringController(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringController);

        // 创建处理通过注解定义的Spring Task信息的类
        WriteDbHandler4SpringTaskJava writeDbHandler4SpringTaskJava = new WriteDbHandler4SpringTaskJava(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringTaskJava);
        // 由于spring_task会分别写入通过XML及注解定义的信息，因此这里需要修改recordId的值
        writeDbHandler4SpringTaskJava.setRecordId(springTaskAnnotationCounter.getCount());

        // 处理方法注解信息，需要在类注解之后处理
        initWriteDbHandler(writeDbHandler4MethodAnnotation);
        writeDbHandler4MethodAnnotation.setClassRequestMappingMap(classRequestMappingMap);
        writeDbHandler4MethodAnnotation.setWriteDbHandler4SpringController(writeDbHandler4SpringController);
        writeDbHandler4MethodAnnotation.setWriteDbHandler4SpringTaskJava(writeDbHandler4SpringTaskJava);
        writeDbHandler4MethodAnnotation.setWithAnnotationMethodHashSet(withAnnotationMethodHashSet);
        if (!writeDbHandler4MethodAnnotation.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理方法参数注解信息
        WriteDbHandler4MethodArgAnnotation writeDbHandler4MethodArgAnnotation = new WriteDbHandler4MethodArgAnnotation(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodArgAnnotation);
        return writeDbHandler4MethodArgAnnotation.handle(javaCG2OutputInfo);
    }

    protected WriteDbHandler4MethodInfo genWriteDbHandler4MethodInfo() {
        return new WriteDbHandler4MethodInfo(writeDbResult);
    }

    protected WriteDbHandler4MethodLineNumber genWriteDbHandler4MethodLineNumber() {
        return new WriteDbHandler4MethodLineNumber(writeDbResult);
    }

    // 处理方法
    private boolean handleMethod(Set<String> withArgsGenericsTypeMethodHash, Set<String> withReturnGenericsTypeMethodHash, Set<Integer> withInfoCallIdSet) {
        // 处理方法信息
        WriteDbHandler4MethodInfo writeDbHandler4MethodInfo = genWriteDbHandler4MethodInfo();
        initWriteDbHandler(writeDbHandler4MethodInfo);
        if (!writeDbHandler4MethodInfo.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4DupMethodInfo writeDbHandler4DupMethodInfo = new WriteDbHandler4DupMethodInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4DupMethodInfo);
        if (!writeDbHandler4DupMethodInfo.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理方法行号
        WriteDbHandler4MethodLineNumber writeDbHandler4MethodLineNumber = genWriteDbHandler4MethodLineNumber();
        initWriteDbHandler(writeDbHandler4MethodLineNumber);
        if (!writeDbHandler4MethodLineNumber.handle(javaCG2OutputInfo)) {
            return false;
        }

        if (useNeo4j()) {
            return true;
        }

        // 处理方法参数
        WriteDbHandler4MethodArgument writeDbHandler4MethodArgument = new WriteDbHandler4MethodArgument(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodArgument);
        if (!writeDbHandler4MethodArgument.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理方法参数泛型类型
        WriteDbHandler4MethodArgGenericsType writeDbHandler4MethodArgGenericsType = new WriteDbHandler4MethodArgGenericsType(writeDbResult);
        writeDbHandler4MethodArgGenericsType.setWithArgsGenericsTypeMethodHashSet(withArgsGenericsTypeMethodHash);
        initWriteDbHandler(writeDbHandler4MethodArgGenericsType);
        if (!writeDbHandler4MethodArgGenericsType.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理方法返回泛型类型
        WriteDbHandler4MethodReturnGenericsType writeDbHandler4MethodReturnGenericsType = new WriteDbHandler4MethodReturnGenericsType(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodReturnGenericsType);
        writeDbHandler4MethodReturnGenericsType.setWithReturnGenericsTypeMethodHash(withReturnGenericsTypeMethodHash);
        if (!writeDbHandler4MethodReturnGenericsType.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理方法调用信息（需要在处理方法调用关系文件之前完成）
        WriteDbHandler4MethodCallInfo writeDbHandler4MethodCallInfo = new WriteDbHandler4MethodCallInfo(writeDbResult);
        writeDbHandler4MethodCallInfo.setWithInfoCallIdSet(withInfoCallIdSet);
        initWriteDbHandler(writeDbHandler4MethodCallInfo);
        if (!writeDbHandler4MethodCallInfo.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理方法调用被调用对象的原始类型
        WriteDbHandler4MethodCallRawCallee writeDbHandler4MethodCallRawCallee = new WriteDbHandler4MethodCallRawCallee(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodCallRawCallee);
        if (!writeDbHandler4MethodCallRawCallee.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理方法调用使用方法调用返回值信息
        WriteDbHandler4MethodCallMethodCallReturn writeDbHandler4MethodCallMethodCallReturn = new WriteDbHandler4MethodCallMethodCallReturn(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodCallMethodCallReturn);
        if (!writeDbHandler4MethodCallMethodCallReturn.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理方法调用使用静态字段信息
        WriteDbHandler4MethodCallStaticField writeDbHandler4MethodCallStaticField = new WriteDbHandler4MethodCallStaticField(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodCallStaticField);
        if (!writeDbHandler4MethodCallStaticField.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理方法调用使用非静态字段信息
        WriteDbHandler4MethodCallNonStaticField writeDbHandler4MethodCallNonStaticField = new WriteDbHandler4MethodCallNonStaticField(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodCallNonStaticField);
        if (!writeDbHandler4MethodCallNonStaticField.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理方法调用（的被调用对象或参数）中使用静态字段的方法调用返回值信息
        WriteDbHandler4MethodCallStaticFieldMCR writeDbHandler4MethodCallStaticFieldMcr = new WriteDbHandler4MethodCallStaticFieldMCR(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodCallStaticFieldMcr);
        if (!writeDbHandler4MethodCallStaticFieldMcr.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 方法返回值对应的方法参数序号
        WriteDbHandler4MethodReturnArgSeq writeDbHandler4MethodReturnArgSeq = new WriteDbHandler4MethodReturnArgSeq(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodReturnArgSeq);
        if (!writeDbHandler4MethodReturnArgSeq.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 方法返回值对应的方法调用序号
        WriteDbHandler4MethodReturnCallId writeDbHandler4MethodReturnCallId = new WriteDbHandler4MethodReturnCallId(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodReturnCallId);
        if (!writeDbHandler4MethodReturnCallId.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 方法返回的常量值（含null）
        WriteDbHandler4MethodReturnConstValue writeDbHandler4MethodReturnConstValue = new WriteDbHandler4MethodReturnConstValue(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodReturnConstValue);
        if (!writeDbHandler4MethodReturnConstValue.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 方法返回的字段（含枚举）
        WriteDbHandler4MethodReturnFieldInfo writeDbHandler4MethodReturnFieldInfo = new WriteDbHandler4MethodReturnFieldInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodReturnFieldInfo);
        if (!writeDbHandler4MethodReturnFieldInfo.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 方法的catch信息
        WriteDbHandler4MethodCatch writeDbHandler4MethodTryCatchFinally = new WriteDbHandler4MethodCatch(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodTryCatchFinally);
        if (!writeDbHandler4MethodTryCatchFinally.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 方法的finally信息
        WriteDbHandler4MethodFinally writeDbHandler4MethodFinally = new WriteDbHandler4MethodFinally(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodFinally);
        if (!writeDbHandler4MethodFinally.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 方法的throw信息
        WriteDbHandler4MethodThrow writeDbHandler4MethodThrow = new WriteDbHandler4MethodThrow(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodThrow);
        return writeDbHandler4MethodThrow.handle(javaCG2OutputInfo);
    }

    // 处理类的信息
    private boolean handleClassInfo(Set<String> enumSimpleClassNameSet) {
        WriteDbHandler4ClassInfo writeDbHandler4ClassInfo = genWriteDbHandler4ClassInfo();
        initWriteDbHandler(writeDbHandler4ClassInfo);
        writeDbHandler4ClassInfo.setEnumSimpleClassNameSet(enumSimpleClassNameSet);
        if (!writeDbHandler4ClassInfo.handle(javaCG2OutputInfo)) {
            return false;
        }

        if (useNeo4j()) {
            return true;
        }

        // 等待直到任务执行完毕，等待类信息写入完毕，后面需要使用
        wait4TPEDone();

        WriteDbHandler4DupClassInfo writeDbHandler4DupClassInfo = new WriteDbHandler4DupClassInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4DupClassInfo);
        if (!writeDbHandler4DupClassInfo.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4InnerClassInfo writeDbHandler4InnerClassInfo = new WriteDbHandler4InnerClassInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4InnerClassInfo);
        if (!writeDbHandler4InnerClassInfo.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4PackageInfo writeDbHandler4PackageInfo = new WriteDbHandler4PackageInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4PackageInfo);
        return writeDbHandler4PackageInfo.handle(javaCG2OutputInfo);
    }

    protected WriteDbHandler4ClassInfo genWriteDbHandler4ClassInfo() {
        return new WriteDbHandler4ClassInfo(writeDbResult);
    }

    // 处理继承与实现相关信息
    private boolean handleExtendsImpl(Map<String, String> extendsSimpleClassNameMap) {
        // 预处理
        WriteDbHandler4ExtendsImplPre writeDbHandler4ExtendsImplPre = new WriteDbHandler4ExtendsImplPre(writeDbResult);
        initWriteDbHandler(writeDbHandler4ExtendsImplPre);
        writeDbHandler4ExtendsImplPre.handle(javaCG2OutputInfo);
        Set<String> superClassOrInterfaceNameSet = writeDbHandler4ExtendsImplPre.getSuperClassOrInterfaceNameSet();

        // 正式处理
        WriteDbHandler4ExtendsImpl writeDbHandler4ExtendsImpl = genWriteDbHandler4ExtendsImpl();
        initWriteDbHandler(writeDbHandler4ExtendsImpl);
        writeDbHandler4ExtendsImpl.setSuperClassOrInterfaceNameSet(superClassOrInterfaceNameSet);
        writeDbHandler4ExtendsImpl.setExtendsSimpleClassNameMap(extendsSimpleClassNameMap);
        return writeDbHandler4ExtendsImpl.handle(javaCG2OutputInfo);
    }

    protected WriteDbHandler4ExtendsImpl genWriteDbHandler4ExtendsImpl() {
        return new WriteDbHandler4ExtendsImpl(writeDbResult);
    }

    // 处理Lambda表达式方法信息
    private boolean handleLambdaMethodInfo() {
        if (useNeo4j()) {
            return true;
        }
        WriteDbHandler4LambdaMethodInfo writeDbHandler4LambdaMethodInfo = new WriteDbHandler4LambdaMethodInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4LambdaMethodInfo);
        return writeDbHandler4LambdaMethodInfo.handle(javaCG2OutputInfo);
    }

    // 处理Spring相关信息（不包括Spring Controller）
    private boolean handleSpringInfo(JavaCG2Counter springTaskAnnotationCounter) {
        if (useNeo4j()) {
            return true;
        }
        // 处理Spring Bean信息
        Map<String, String> springBeanMap = new HashMap<>();
        WriteDbHandler4SpringBean writeDbHandler4SpringBean = new WriteDbHandler4SpringBean(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringBean);
        writeDbHandler4SpringBean.setSpringBeanMap(springBeanMap);
        if (!writeDbHandler4SpringBean.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理Spring Task信息
        WriteDbHandler4SpringTaskXml writeDbHandler4SpringTask = new WriteDbHandler4SpringTaskXml(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringTask);
        writeDbHandler4SpringTask.setSpringBeanMap(springBeanMap);
        if (!writeDbHandler4SpringTask.handle(javaCG2OutputInfo)) {
            return false;
        }
        springTaskAnnotationCounter.setCount(writeDbHandler4SpringTask.getRecordId());

        // 处理Spring包扫描路径
        Set<String> scanPackageSet = new HashSet<>();
        WriteDbHandler4SpringScanPackageXml writeDbHandler4SpringScanPackageXml = new WriteDbHandler4SpringScanPackageXml(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringScanPackageXml);
        writeDbHandler4SpringScanPackageXml.setScanPackageSet(scanPackageSet);
        if (!writeDbHandler4SpringScanPackageXml.handle(javaCG2OutputInfo)) {
            return false;
        }
        WriteDbHandler4SpringScanPackageJava writeDbHandler4SpringScanPackageJava = new WriteDbHandler4SpringScanPackageJava(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringScanPackageJava);
        writeDbHandler4SpringScanPackageJava.setRecordId(writeDbHandler4SpringScanPackageXml.getRecordId());
        writeDbHandler4SpringScanPackageJava.setScanPackageSet(scanPackageSet);
        if (!writeDbHandler4SpringScanPackageJava.handle(javaCG2OutputInfo)) {
            return false;
        }

        if (!parseSpringAopInfo) {
            return true;
        }

        logger.info("对Spring AOP相关信息进行解析");

        // 处理Spring AOP aspect
        WriteDbHandler4SpringAopAspectJava writeDbHandler4SpringAopAspectJava = new WriteDbHandler4SpringAopAspectJava(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringAopAspectJava);
        if (!writeDbHandler4SpringAopAspectJava.handle(javaCG2OutputInfo)) {
            return false;
        }
        WriteDbHandler4SpringAopAspectXml writeDbHandler4SpringAopAspectXml = new WriteDbHandler4SpringAopAspectXml(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringAopAspectXml);
        writeDbHandler4SpringAopAspectXml.setRecordId(writeDbHandler4SpringAopAspectJava.getRecordId());
        writeDbHandler4SpringAopAspectXml.setSpringBeanMap(springBeanMap);
        if (!writeDbHandler4SpringAopAspectXml.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理Spring AOP pointcut
        WriteDbHandler4SpringAopPointcutJava writeDbHandler4SpringAopPointcutJava = new WriteDbHandler4SpringAopPointcutJava(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringAopPointcutJava);
        if (!writeDbHandler4SpringAopPointcutJava.handle(javaCG2OutputInfo)) {
            return false;
        }
        WriteDbHandler4SpringAopPointcutXml writeDbHandler4SpringAopPointcutXml = new WriteDbHandler4SpringAopPointcutXml(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringAopPointcutXml);
        writeDbHandler4SpringAopPointcutXml.setRecordId(writeDbHandler4SpringAopPointcutJava.getRecordId());
        if (!writeDbHandler4SpringAopPointcutXml.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理Spring AOP advice
        WriteDbHandler4SpringAopAdviceJava writeDbHandler4SpringAopAdviceJava = new WriteDbHandler4SpringAopAdviceJava(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringAopAdviceJava);
        if (!writeDbHandler4SpringAopAdviceJava.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 等待前面的Spring AOP数据写入完毕
        wait4TPEDone();

        WriteDbHandler4SpringAopAdviceXml writeDbHandler4SpringAopAdviceXml = new WriteDbHandler4SpringAopAdviceXml(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringAopAdviceXml);
        writeDbHandler4SpringAopAdviceXml.setRecordId(writeDbHandler4SpringAopAdviceJava.getRecordId());
        return writeDbHandler4SpringAopAdviceXml.handle(javaCG2OutputInfo);
    }

    // 处理MyBatis信息
    private boolean handleMyBatisInfo(Set<String> myBatisMapperMethodSet, Set<String> myBatisMapperMethodWriteSet) {
        if (useNeo4j()) {
            return true;
        }
        // 处理MyBatis Mapper方法操作的数据库表信息
        WriteDbHandler4MyBatisMSTable writeDbHandler4MyBatisMSTable = new WriteDbHandler4MyBatisMSTable(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSTable);
        writeDbHandler4MyBatisMSTable.setMyBatisMapperMethodSet(myBatisMapperMethodSet);
        if (!writeDbHandler4MyBatisMSTable.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理MyBatis Mapper方法写的数据库表信息
        WriteDbHandler4MyBatisMSWriteTable writeDbHandler4MyBatisMSWriteTable = new WriteDbHandler4MyBatisMSWriteTable(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSWriteTable);
        writeDbHandler4MyBatisMSWriteTable.setMyBatisMapperMethodWriteSet(myBatisMapperMethodWriteSet);
        if (!writeDbHandler4MyBatisMSWriteTable.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理MyBatis的Entity与数据库字段名信息
        WriteDbHandler4MybatisMSColumn writeDbHandler4MybatisMSColumn = new WriteDbHandler4MybatisMSColumn(writeDbResult);
        initWriteDbHandler(writeDbHandler4MybatisMSColumn);
        if (!writeDbHandler4MybatisMSColumn.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理MyBatis的Entity与Mapper、表名
        WriteDbHandler4MybatisMSEntity writeDbHandler4MybatisMSEntity = new WriteDbHandler4MybatisMSEntity(writeDbResult);
        initWriteDbHandler(writeDbHandler4MybatisMSEntity);
        if (!writeDbHandler4MybatisMSEntity.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理MyBatis的格式化后的sql文本
        WriteDbHandler4MyBatisMSFormatedSql writeDbHandler4MyBatisMSFormatedSql = new WriteDbHandler4MyBatisMSFormatedSql(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSFormatedSql);
        if (!writeDbHandler4MyBatisMSFormatedSql.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理MyBatis的update set子句的字段信息
        WriteDbHandler4MyBatisMSSetColumn writeDbHandler4MyBatisMSSetColumn = new WriteDbHandler4MyBatisMSSetColumn(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSSetColumn);
        if (!writeDbHandler4MyBatisMSSetColumn.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理MyBatis的where子句的字段信息
        WriteDbHandler4MyBatisMSWhereColumn writeDbHandler4MyBatisMSWhereColumn = new WriteDbHandler4MyBatisMSWhereColumn(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSWhereColumn);
        if (!writeDbHandler4MyBatisMSWhereColumn.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 处理MyBatis的select的字段信息
        WriteDbHandler4MyBatisMSSelectColumn writeDbHandler4MyBatisMSSelectColumn = new WriteDbHandler4MyBatisMSSelectColumn(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSSelectColumn);
        if (!writeDbHandler4MyBatisMSSelectColumn.handle(javaCG2OutputInfo)) {
            return false;
        }

        // 等待MyBatis相关数据写入完毕
        wait4TPEDone();

        // 记录MyBatis XML中返回的resultMap内容的HASH值（支持MySQL）
        return recordMyBatisMSReturnResultMapHash();
    }

    // 处理类的泛型相关信息
    private boolean handleClassGenericsType() {
        if (useNeo4j()) {
            return true;
        }

        WriteDbHandler4ClassSignatureGenericsType writeDbHandler4ClassSignatureGenericsType = new WriteDbHandler4ClassSignatureGenericsType(writeDbResult);
        initWriteDbHandler(writeDbHandler4ClassSignatureGenericsType);
        if (!writeDbHandler4ClassSignatureGenericsType.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4ClassExtImplGenericsType writeDbHandler4ClassExtImplGenericsType = new WriteDbHandler4ClassExtImplGenericsType(writeDbResult);
        initWriteDbHandler(writeDbHandler4ClassExtImplGenericsType);
        return writeDbHandler4ClassExtImplGenericsType.handle(javaCG2OutputInfo);
    }

    protected WriteDbHandler4GetMethod genWriteDbHandler4GetMethod() {
        return new WriteDbHandler4GetMethod(writeDbResult);
    }

    protected WriteDbHandler4SetMethod genWriteDbHandler4SetMethod() {
        return new WriteDbHandler4SetMethod(writeDbResult);
    }

    // 处理get/set方法，以对应的调用关系
    private boolean handleFieldRelationship(Map<String, String> extendsSimpleClassNameMap, Set<String> enumSimpleClassNameSet, Map<String, Set<String>> getMethodSimpleClassMap,
                                            Map<String, Set<String>> setMethodSimpleClassMap) {
        WriteDbHandler4GetMethod writeDbHandler4GetMethod = genWriteDbHandler4GetMethod();
        initWriteDbHandler(writeDbHandler4GetMethod);
        writeDbHandler4GetMethod.setGetMethodSimpleClassMap(getMethodSimpleClassMap);
        if (!writeDbHandler4GetMethod.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4SetMethod writeDbHandler4SetMethod = genWriteDbHandler4SetMethod();
        initWriteDbHandler(writeDbHandler4SetMethod);
        writeDbHandler4SetMethod.setSetMethodSimpleClassMap(setMethodSimpleClassMap);
        if (!writeDbHandler4SetMethod.handle(javaCG2OutputInfo)) {
            return false;
        }

        if (useNeo4j()) {
            return true;
        }
        WriteDbHandler4FieldRelationship writeDbHandler4FieldRelationship = new WriteDbHandler4FieldRelationship(writeDbResult);
        initWriteDbHandler(writeDbHandler4FieldRelationship);
        writeDbHandler4FieldRelationship.setGetMethodSimpleClassMap(getMethodSimpleClassMap);
        writeDbHandler4FieldRelationship.setSetMethodSimpleClassMap(setMethodSimpleClassMap);
        writeDbHandler4FieldRelationship.setExtendsSimpleClassNameMap(extendsSimpleClassNameMap);
        writeDbHandler4FieldRelationship.setEnumSimpleClassNameSet(enumSimpleClassNameSet);
        return writeDbHandler4FieldRelationship.handle(javaCG2OutputInfo);
    }

    // 处理配置文件
    private boolean handleConfFile() {
        if (useNeo4j()) {
            return true;
        }
        WriteDbHandler4PropertiesConf writeDbHandler4PropertiesConf = new WriteDbHandler4PropertiesConf(writeDbResult);
        initWriteDbHandler(writeDbHandler4PropertiesConf);
        return writeDbHandler4PropertiesConf.handle(javaCG2OutputInfo);
    }

    // 处理解析jar文件时获取的自定义数据
    private boolean handleParsedCustomData() {
        if (useNeo4j()) {
            return true;
        }
        WriteDbHandler4ParsedCustomData writeDbHandler4ParsedCustomData = new WriteDbHandler4ParsedCustomData(writeDbResult);
        initWriteDbHandler(writeDbHandler4ParsedCustomData);
        return writeDbHandler4ParsedCustomData.handle(javaCG2OutputInfo);
    }

    // 处理通过方法调用传递的get/set方法关联关系（需要在处理方法调用关系后面执行）
    private boolean handleMethodCallPassedGetSet() {
        if (useNeo4j()) {
            return true;
        }

        boolean analyseFieldRelationship = javaCG2ConfigureWrapper.getMainConfig(JavaCG2ConfigKeyEnum.CKE_ANALYSE_FIELD_RELATIONSHIP);
        if (!analyseFieldRelationship) {
            logger.info("不解析dto的字段之间通过get/set方法的关联关系时，跳过对应处理");
            return true;
        }

        logger.info("解析dto的字段之间通过get/set方法的关联关系，处理通过方法调用传递的字段关联关系");
        WriteDbHandler4SetMethodAssignInfo writeDbHandler4SetMethodAssignInfo = new WriteDbHandler4SetMethodAssignInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4SetMethodAssignInfo);

        MethodCallPassedFieldRelationshipHandler methodCallPassedFieldRelationshipHandler = new MethodCallPassedFieldRelationshipHandler(dbOperWrapper);
        methodCallPassedFieldRelationshipHandler.setWriteDbHandler4SetMethodAssignInfo(writeDbHandler4SetMethodAssignInfo);
        return methodCallPassedFieldRelationshipHandler.handle(javaCG2OutputInfo.getOutputDirPath());
    }

    // 处理MyBatis XML文件中sql脚本的字段与Java代码的关联关系（使用MySQL）
    private boolean handleMyBatisMSJavaColumn() {
        if (useNeo4j()) {
            return true;
        }
        WriteDbHandler4MybatisMSGetSetDb writeDbHandler4MybatisMSGetSetDb = new WriteDbHandler4MybatisMSGetSetDb(writeDbResult);
        initWriteDbHandler(writeDbHandler4MybatisMSGetSetDb);

        MyBatisMSJavaColumnHandler myBatisMSJavaColumnHandler = new MyBatisMSJavaColumnHandler(dbOperWrapper);
        myBatisMSJavaColumnHandler.setWriteDbHandler4MybatisMSGetSetDb(writeDbHandler4MybatisMSGetSetDb);
        return myBatisMSJavaColumnHandler.handle(javaCG2OutputInfo.getOutputDirPath());
    }

    // 处理Spring AOP advice
    private boolean handleSpringAopAdvice() {
        if (useNeo4j() || !parseSpringAopInfo) {
            return true;
        }

        // Spring AOP advice Around信息
        WriteDbHandler4SpringAopAdviceAround writeDbHandler4SpringAopAdviceAround = new WriteDbHandler4SpringAopAdviceAround(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringAopAdviceAround);
        return writeDbHandler4SpringAopAdviceAround.handle(javaCG2OutputInfo.getOutputDirPath());
    }

    // 处理方法调用关系文件
    private boolean handleMethodCall(Map<String, String> extendsSimpleClassNameMap,
                                     Set<String> withAnnotationMethodHashSet,
                                     Set<Integer> withInfoCallIdSet,
                                     Set<String> withArgsGenericsTypeMethodHashSet,
                                     Set<String> withReturnGenericsTypeMethodHashSet,
                                     Set<String> myBatisMapperMethodSet,
                                     Set<String> myBatisMapperMethodWriteSet,
                                     Map<String, Set<String>> getMethodSimpleClassMap,
                                     Map<String, Set<String>> setMethodSimpleClassMap) {
        // 写入数据库，方法调用关系相关类
        WriteDbHandler4MethodCall writeDbHandler4MethodCall = genWriteDbHandler4MethodCall();
        initWriteDbHandler(writeDbHandler4MethodCall);
        writeDbHandler4MethodCall.setExtendsSimpleClassNameMap(extendsSimpleClassNameMap);
        writeDbHandler4MethodCall.setWithAnnotationMethodHashSet(withAnnotationMethodHashSet);
        writeDbHandler4MethodCall.setWithInfoCallIdSet(withInfoCallIdSet);
        writeDbHandler4MethodCall.setWithArgsGenericsTypeMethodHashSet(withArgsGenericsTypeMethodHashSet);
        writeDbHandler4MethodCall.setWithReturnGenericsTypeMethodHashSet(withReturnGenericsTypeMethodHashSet);
        writeDbHandler4MethodCall.setMyBatisMapperMethodSet(myBatisMapperMethodSet);
        writeDbHandler4MethodCall.setMyBatisMapperMethodWriteSet(myBatisMapperMethodWriteSet);
        writeDbHandler4MethodCall.setGetMethodSimpleClassMap(getMethodSimpleClassMap);
        writeDbHandler4MethodCall.setSetMethodSimpleClassMap(setMethodSimpleClassMap);
        writeDbHandler4MethodCall.recordJacgMethodCallExtensionList(jacgMethodCallExtensionList);
        return writeDbHandler4MethodCall.handle(javaCG2OutputInfo);
    }

    protected WriteDbHandler4MethodCall genWriteDbHandler4MethodCall() {
        return new WriteDbHandler4MethodCall(writeDbResult);
    }

    // 处理字段
    private boolean handleField() {
        if (useNeo4j()) {
            return true;
        }
        WriteDbHandler4FieldAnnotation writeDbHandler4FieldAnnotation = new WriteDbHandler4FieldAnnotation(writeDbResult);
        initWriteDbHandler(writeDbHandler4FieldAnnotation);
        if (!writeDbHandler4FieldAnnotation.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4FieldInfo writeDbHandler4FieldInfo = new WriteDbHandler4FieldInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4FieldInfo);
        if (!writeDbHandler4FieldInfo.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4DupFieldInfo writeDbHandler4DupFieldInfo = new WriteDbHandler4DupFieldInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4DupFieldInfo);
        if (!writeDbHandler4DupFieldInfo.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4FieldUsageOther writeDbHandler4FieldUsageOther = new WriteDbHandler4FieldUsageOther(writeDbResult);
        initWriteDbHandler(writeDbHandler4FieldUsageOther);
        if (!writeDbHandler4FieldUsageOther.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4SfFieldMethodCall writeDbHandler4SfFieldMethodCall = new WriteDbHandler4SfFieldMethodCall(writeDbResult);
        initWriteDbHandler(writeDbHandler4SfFieldMethodCall);
        if (!writeDbHandler4SfFieldMethodCall.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4FieldGenericsType writeDbHandler4FieldGenericsType = new WriteDbHandler4FieldGenericsType(writeDbResult);
        initWriteDbHandler(writeDbHandler4FieldGenericsType);
        if (!writeDbHandler4FieldGenericsType.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4EnumInitArgField writeDbHandler4EnumInitArgField = new WriteDbHandler4EnumInitArgField(writeDbResult);
        initWriteDbHandler(writeDbHandler4EnumInitArgField);
        if (!writeDbHandler4EnumInitArgField.handle(javaCG2OutputInfo)) {
            return false;
        }

        WriteDbHandler4EnumInitAssignInfo writeDbHandler4EnumInitAssignInfo = new WriteDbHandler4EnumInitAssignInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4EnumInitAssignInfo);
        return writeDbHandler4EnumInitAssignInfo.handle(javaCG2OutputInfo);
    }

    // 人工添加方法调用关系（需要在方法调用关系文件处理完毕后执行）
    private boolean manualAddMethodCall() {
        if (useNeo4j()) {
            return true;
        }
        for (AbstractManualAddMethodCall1 manualAddMethodCall1 : manualAddMethodCall1List) {
            if (!manualAddMethodCall1.addMethodCall()) {
                return false;
            }
        }
        return true;
    }

    // 显示H2数据库JDBC URL
    private void printH2JdbcUrl() {
        String h2DbFilePath = JavaCG2FileUtil.getCanonicalPath(getH2DbFile());
        if (h2DbFilePath == null) {
            return;
        }
        String h2DbFilePathWithoutExt = JACGFileUtil.getFileNameWithOutExt(h2DbFilePath, JACGConstants.H2_FILE_EXT);
        logger.info("可用于连接H2数据库的JDBC URL与文件路径:\n{}{}\n{}", JACGConstants.H2_PROTOCOL, h2DbFilePathWithoutExt, h2DbFilePath);
    }

    // 检查执行结果
    protected boolean checkResult() {
        int failTimes = 0;
        // 执行写入数据库的类执行完成之后的操作
        Map<String, AbstractWriteDbHandler<?>> writeDbHandlerMap = writeDbResult.getWriteDbHandlerMap();
        List<String> writeDbHandlerNameList = new ArrayList<>(writeDbHandlerMap.keySet());
        Collections.sort(writeDbHandlerNameList);
        for (String writeDbHandlerName : writeDbHandlerNameList) {
            AbstractWriteDbHandler<?> writeDbHandler = writeDbHandlerMap.get(writeDbHandlerName);
            if (!writeDbHandler.finalCheck() || writeDbHandler.checkFailed()) {
                failTimes++;
            }
        }
        if (failTimes > 0) {
            return false;
        }

        Map<String, JavaCG2Counter> writeDbNumMap = writeDbResult.getWriteDbNumMap();
        List<String> writeDbNameList = new ArrayList<>(writeDbNumMap.keySet());
        Collections.sort(writeDbNameList);
        for (String writeDbName : writeDbNameList) {
            int writeDbNum = writeDbNumMap.get(writeDbName).getCount();
            logger.info("{} 写入数据库表记录数 {}", writeDbName, writeDbNum);
            if (DbTableInfoEnum.DTIE_CLASS_NAME.getTableNameKeyword().equals(writeDbName) && writeDbNum == 0) {
                logger.warn("未向数据库写入数据，请检查方法调用文件是否为空\n{}", javaCG2OutputInfo.getMainFilePath(JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL));
            }
        }

        Map<String, JavaCG2Counter> writeFileNumMap = writeDbResult.getWriteFileNumMap();
        List<String> writeFileNameList = new ArrayList<>(writeFileNumMap.keySet());
        Collections.sort(writeFileNameList);
        for (String writeFileName : writeFileNameList) {
            logger.info("{} 写入文件记录数 {}", writeFileName, writeFileNumMap.get(writeFileName).getCount());
        }

        Map<String, JavaCG2Counter> failNumMap = writeDbResult.getFailNumMap();
        for (Map.Entry<String, JavaCG2Counter> entry : failNumMap.entrySet()) {
            if (entry.getValue().getCount() > 0) {
                logger.error("写数据库操作失败 {}", entry.getKey());
                return false;
            }
        }
        return true;
    }

    // 记录MyBatis XML中返回的resultMap内容的HASH值（支持MySQL）
    private boolean recordMyBatisMSReturnResultMapHash() {
        // 从mybatis_ms_formated_sql表查询所有的XML文件名
        String querySql1 = "select distinct " + DC.MMFS_XML_FILE_NAME +
                " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL.getTableName();
        querySql1 = dbOperWrapper.cacheSql(SqlKeyEnum.MMFS_QUERY_ALL_XML_FILE_NAME, querySql1);
        List<String> xmlFileNameList = dbOperator.queryListOneColumn(querySql1, String.class);
        if (JavaCG2Util.isCollectionEmpty(xmlFileNameList)) {
            logger.info("查询 {} 表记录为空", DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL.getTableNameKeyword());
            return true;
        }

        String querySql2 = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL) +
                " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL.getTableName() +
                " where " + DC.MMFS_XML_FILE_NAME + " = ?";
        querySql2 = dbOperWrapper.cacheSql(SqlKeyEnum.MMFS_QUERY_BY_XML_FILE_NAME, querySql2);

        String querySql3 = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_MYBATIS_MS_COLUMN) +
                " from " + DbTableInfoEnum.DTIE_MYBATIS_MS_COLUMN.getTableName() +
                " where " + DC.MMC_XML_FILE_NAME + " = ?" +
                "and " + DC.MMC_XML_FILE_PATH + " = ?" +
                "and " + DC.MMC_RESULT_MAP_ID + " = ?";
        querySql3 = dbOperWrapper.cacheSql(SqlKeyEnum.MMC_QUERY_BY_XML_RESULT_MAP_ID, querySql3);

        String updateSql1 = "update " + DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL.getTableName() +
                " set " + DC.MMFS_RESULT_MAP_HASH + " = ?" +
                " where " + DC.MMFS_RECORD_ID + " = ?";
        updateSql1 = dbOperWrapper.cacheSql(SqlKeyEnum.MMFS_UPDATE_RESULT_MAP_HASH, updateSql1);

        for (String xmlFileName : xmlFileNameList) {
            // 根据XML文件名从mybatis_ms_formated_sql表查询对应的记录
            List<WriteDbData4MyBatisMSFormatedSql> myBatisMSFormatedSqlList = dbOperator.queryList(querySql2, WriteDbData4MyBatisMSFormatedSql.class, xmlFileName);
            if (JavaCG2Util.isCollectionEmpty(xmlFileNameList)) {
                continue;
            }
            for (WriteDbData4MyBatisMSFormatedSql myBatisMSFormatedSql : myBatisMSFormatedSqlList) {
                if (StringUtils.isBlank(myBatisMSFormatedSql.getResultMapId())) {
                    continue;
                }
                // 查询mybatis_ms_formated_sql表当前的XML元素返回的resultMap的信息
                List<WriteDbData4MybatisMSColumn> mybatisMSColumnList = dbOperator.queryList(querySql3, WriteDbData4MybatisMSColumn.class,
                        myBatisMSFormatedSql.getXmlFileName(), myBatisMSFormatedSql.getXmlFilePath(),
                        myBatisMSFormatedSql.getResultMapId());
                if (JavaCG2Util.isCollectionEmpty(mybatisMSColumnList)) {
                    continue;
                }
                List<String> mybatisMSColumnValueList = new ArrayList<>(mybatisMSColumnList.size());
                for (WriteDbData4MybatisMSColumn mybatisMSColumn : mybatisMSColumnList) {
                    mybatisMSColumnValueList.add(StringUtils.joinWith(JACGConstants.FLAG_SPACE, mybatisMSColumn.getEntityFieldName(), mybatisMSColumn.getColumnName(),
                            mybatisMSColumn.getColumnType()));
                }
                Collections.sort(mybatisMSColumnValueList);
                String resultMapValue = StringUtils.join(mybatisMSColumnValueList, JavaCG2Constants.NEW_LINE);
                String resultMapHash = JACGUtil.genHashWithLen(resultMapValue);
                // 更新mybatis_ms_formated_sql表的resultMap内容hash+字节数
                Integer updatedRowNum = dbOperator.update(updateSql1, resultMapHash, myBatisMSFormatedSql.getRecordId());
                if (updatedRowNum != 1) {
                    logger.error("更新 {} 表时返回行数不是1 {}", DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL.getTableNameKeyword(), updatedRowNum);
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    protected boolean handleDb() {
        // 返回需要操作数据库
        return true;
    }

    /**
     * 跳过调用java-callgraph2的步骤
     *
     * @param javaCG2OutputInfo
     * @param currentOutputDirPath
     */
    public void configSkipCallJavaCG2(JavaCG2OutputInfo javaCG2OutputInfo, String currentOutputDirPath) {
        this.skipCallJavaCG2 = true;
        this.javaCG2OutputInfo = javaCG2OutputInfo;
        this.currentOutputDirPath = currentOutputDirPath;
    }

    public int getWriteDbTimes() {
        return writeDbTimes;
    }
}

