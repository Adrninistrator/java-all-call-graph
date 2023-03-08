package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.annotation.attributes.AnnotationAttributesFormatter;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringTask;
import com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file.SpringTaskCodeParser;
import com.adrninistrator.jacg.extensions.extended_data_add.ExtendedDataAddInterface;
import com.adrninistrator.jacg.extensions.method_call_add.AbstractMethodCallAdd4ExtendsImpl;
import com.adrninistrator.jacg.extensions.method_call_add.MethodCallAddInterface;
import com.adrninistrator.jacg.handler.write_db.AbstractWriteDbHandler;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ClassAnnotation;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ClassInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ClassName;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ClassSignatureEi1;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ExtendedData;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ExtendsImpl;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ExtendsImplPre;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4JarInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4LambdaMethodInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodAnnotation;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodCall;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodCallInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodLineNumber;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4SpringBean;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4SpringController;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4SpringTask;
import com.adrninistrator.jacg.runner.base.AbstractRunner;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseListEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseSetEnum;
import com.adrninistrator.javacg.conf.JavaCGConfigureWrapper;
import com.adrninistrator.javacg.dto.output.HandleOutputInfo;
import com.adrninistrator.javacg.extensions.code_parser.CodeParserInterface;
import com.adrninistrator.javacg.stat.JCallGraph;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description: 生成Java方法调用关系并写入数据库
 */

public class RunnerWriteDb extends AbstractRunner {
    private static final Logger logger = LoggerFactory.getLogger(RunnerWriteDb.class);

    // 需要处理的包名/类名前缀
    private Set<String> allowedClassPrefixSet;

    // java-callgraph2输出文件信息
    private HandleOutputInfo handleOutputInfo;

    // java-callgraph2的配置包装
    private JavaCGConfigureWrapper javaCGConfigureWrapper;

    /*
        记录写入数据库的对象
        key
            写入数据库的简单类名
        value
            写入数据库的对象
     */
    private final Map<String, AbstractWriteDbHandler<?>> writeDbHandlerMap = new HashMap<>();

    // 人工添加方法调用关系类列表
    private List<MethodCallAddInterface> methodCallAddExtList;

    // 保存用于根据方法调用信息添加方法调用自定义数据的处理类
    private List<ExtendedDataAddInterface> extendedDataAddExtList;

    private JCallGraph jCallGraph;

    public boolean run(ConfigureWrapper configureWrapper, JavaCGConfigureWrapper javaCGConfigureWrapper) {
        this.javaCGConfigureWrapper = javaCGConfigureWrapper;
        return run(configureWrapper);
    }

    @Override
    public boolean preHandle() {
        if (JACGSqlUtil.isMySQLDb(confInfo.getDbDriverName()) &&
                !confInfo.getDbUrl().contains(JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS)) {
            logger.info("使用MYSQL时，请在{}参数指定{}", ConfigDbKeyEnum.CDKE_DB_URL.getKey(), JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS);
            return false;
        }

        // 使用多线程，线程数固定为10
        confInfo.setThreadNum(10);

        // 读取其他配置文件
        allowedClassPrefixSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX, true);
        if (allowedClassPrefixSet.isEmpty()) {
            logger.info("所有包中的class文件都需要处理");
        } else {
            logger.warn("仅处理以下包中的class文件\n{}", StringUtils.join(allowedClassPrefixSet, "\n"));
        }
        return true;
    }

    @Override
    public void handle() {
        // 执行实际处理
        if (!operate()) {
            // 记录执行失败
            recordTaskFail();
        }
    }

    @Override
    protected boolean checkH2DbFile() {
        File h2DbFile = getH2DbFile();
        if (!h2DbFile.exists()) {
            return true;
        }

        // 数据库文件存在
        if (!h2DbFile.isFile()) {
            logger.error("H2数据库文件不是文件 {}", JACGFileUtil.getCanonicalPath(h2DbFile));
            return false;
        }

        // 检查H2数据库文件是否可写
        return checkH2DbFileWritable(h2DbFile);
    }

    // 执行实际处理
    private boolean operate() {
        // 创建数据库表
        if (!createTables()) {
            return false;
        }

        // 清理数据库表
        if (!truncateTables()) {
            return false;
        }

        // 添加用于人工添加方法调用关系的处理类
        if (!addMethodCallAddExtensions()) {
            return false;
        }

        // 添加用于根据方法调用信息添加方法调用自定义数据处理类
        if (!addExtendedDataAddExtensions()) {
            return false;
        }

        // 调用java-callgraph2生成jar包的方法调用关系
        if (!callJavaCallGraph2()) {
            return false;
        }

        // 创建线程，参数指定为null，不调小实际创建的线程数
        createThreadPoolExecutor(null);

        // 处理引用的类信息，需要首先处理
        if (!handleClassName()) {
            return false;
        }

        // 处理jar包信息
        handleJarInfo();

        // 处理注解信息
        WriteDbHandler4MethodAnnotation writeDbHandler4MethodAnnotation = new WriteDbHandler4MethodAnnotation();
        handleAnnotations(writeDbHandler4MethodAnnotation);

        // 处理方法行号
        handleMethodLineNumber();

        // 处理类的信息
        handleClassInfo();

        // 处理继承与实现相关信息
        handleExtendsImpl();

        // 处理方法的信息
        handleMethodInfo();

        // 处理Lambda表达式方法信息
        handleLambdaMethodInfo();

        // 处理Spring Bean信息
        handleSpringBean();

        // 类的签名中涉及继承与实现的信息1
        handleClassSignatureEi1();

        // 处理方法调用信息（需要在处理方法调用关系文件之前完成）
        WriteDbHandler4MethodCallInfo writeDbHandler4MethodCallInfo = new WriteDbHandler4MethodCallInfo();
        handleMethodCallInfo(writeDbHandler4MethodCallInfo);

        // 处理方法调用关系文件
        handleMethodCall(writeDbHandler4MethodAnnotation, writeDbHandler4MethodCallInfo);

        if (confInfo.isDbUseH2()) {
            // 显示H2数据库JDBC URL
            printH2JdbcUrl();
        }

        // 检查执行结果
        if (!checkResult()) {
            return false;
        }

        // 打印重复的类名
        printDuplicateClasses();

        return true;
    }

    // 判断是否需要调用java-callgraph2生成jar包的方法调用关系
    private boolean callJavaCallGraph2() {
        List<String> jarPathList = getJarPathList();
        if (JavaCGUtil.isCollectionEmpty(jarPathList)) {
            logger.error("请在配置文件 {} 中指定需要处理的jar包，或保存class、jar文件的目录", OtherConfigFileUseListEnum.OCFULE_JAR_DIR.getKey());
            return false;
        }

        for (String jarPath : jarPathList) {
            if (!new File(jarPath).exists()) {
                logger.error("文件或目录不存在 {}", jarPath);
                return false;
            }
        }

        // 生成java-callgraph2使用的配置信息
        genJavaCGConfigureWrapper(jarPathList);

        jCallGraph = new JCallGraph();
        // 设置对注解属性进行格式化的类
        jCallGraph.setAnnotationAttributesFormatter(new AnnotationAttributesFormatter());

        // 添加用于对代码进行解析的处理类
        addCodeParserExtensions();

        // 调用java-callgraph2
        boolean success = jCallGraph.run(javaCGConfigureWrapper);
        if (!success) {
            logger.error("调用java-callgraph2生成jar包的方法调用关系失败");
            return false;
        }

        // 获取输出信息
        handleOutputInfo = jCallGraph.getHandleOutputInfo();

        // 打印当前使用的配置信息
        printUsedConfigInfo(handleOutputInfo.getOutputDirPath());

        // 操作完成之前的处理
        beforeDone();

        return true;
    }

    // 生成java-callgraph2使用的配置信息
    private void genJavaCGConfigureWrapper(List<String> jarPathList) {
        if (javaCGConfigureWrapper == null) {
            javaCGConfigureWrapper = new JavaCGConfigureWrapper();
        }
        // 指定需要处理的jar包与目录
        javaCGConfigureWrapper.setOtherConfigList(JavaCGOtherConfigFileUseListEnum.OCFULE_JAR_DIR, jarPathList);
        // 指定需要处理的包名
        javaCGConfigureWrapper.setOtherConfigSet(JavaCGOtherConfigFileUseSetEnum.OCFUSE_PACKAGES, allowedClassPrefixSet);
    }

    // 添加用于对代码进行解析的处理类
    private boolean addCodeParserExtensions() {
        List<String> codeParserExtensionClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER, true);
        if (JavaCGUtil.isCollectionEmpty(codeParserExtensionClassList)) {
            logger.info("未指定用于对代码进行解析的类，跳过 {}", OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_CODE_PARSER.getKey());
            return true;
        }

        logger.info("指定用于对代码进行解析的类\n{}", StringUtils.join(codeParserExtensionClassList, "\n"));
        try {
            for (String extensionClass : codeParserExtensionClassList) {
                CodeParserInterface codeParser = JACGUtil.getClassObject(extensionClass, CodeParserInterface.class);
                if (codeParser == null) {
                    return false;
                }

                codeParser.initCodeParser();
                jCallGraph.addCodeParser(codeParser);
            }
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
        return true;
    }

    // 添加用于根据方法调用信息添加方法调用关系的处理类
    private boolean addMethodCallAddExtensions() {
        List<String> methodCallAddClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_CALL_ADD, true);
        if (JavaCGUtil.isCollectionEmpty(methodCallAddClassList)) {
            logger.info("未指定用于人工添加方法调用关系的处理类，跳过 {}", OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_CALL_ADD.getKey());
            methodCallAddExtList = Collections.emptyList();
            return true;
        }

        logger.info("指定用于人工添加方法调用关系的处理类\n{}", StringUtils.join(methodCallAddClassList, "\n"));
        methodCallAddExtList = new ArrayList<>(methodCallAddClassList.size());
        try {
            for (String methodCallAddClass : methodCallAddClassList) {
                MethodCallAddInterface methodCallAddInterface = JACGUtil.getClassObject(methodCallAddClass, MethodCallAddInterface.class);
                if (methodCallAddInterface == null) {
                    return false;
                }

                if (methodCallAddInterface instanceof AbstractMethodCallAdd4ExtendsImpl) {
                    // 设置继承与实际相关的处理类
                    ((AbstractMethodCallAdd4ExtendsImpl) methodCallAddInterface).setJacgExtendsImplHandler(jacgExtendsImplHandler);
                }

                methodCallAddExtList.add(methodCallAddInterface);
            }
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
        return true;
    }

    // 用于根据方法调用信息添加方法调用自定义数据处理类
    private boolean addExtendedDataAddExtensions() {
        List<String> extendedDataAddClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_EXTENDED_DATA_ADD, true);
        if (JavaCGUtil.isCollectionEmpty(extendedDataAddClassList)) {
            logger.info("未指定用于根据方法调用信息添加方法调用自定义数据处理类，跳过 {}", OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_EXTENDED_DATA_ADD.getKey());
            return true;
        }

        extendedDataAddExtList = new ArrayList<>(extendedDataAddClassList.size());
        try {
            for (String extensionClass : extendedDataAddClassList) {
                ExtendedDataAddInterface extendedDataAddExt = JACGUtil.getClassObject(extensionClass, ExtendedDataAddInterface.class);
                if (extendedDataAddExt == null) {
                    return false;
                }

                extendedDataAddExt.setConfInfo(confInfo);
                extendedDataAddExt.initExtendedDataAddInterface();
                extendedDataAddExtList.add(extendedDataAddExt);
            }

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 操作完成之前的处理
    private void beforeDone() {
    }

    // 创建数据库表
    private boolean createTables() {
        logger.info("创建数据库表");
        for (DbTableInfoEnum dbTableInfoEnum : DbTableInfoEnum.values()) {
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
        String sqlFilePath = ConfManager.getInputRootPath() + InputDirEnum.IDE_SQL.getDirName() + "/" + sqlFileName;
        List<String> sqlList = JACGFileUtil.readFile2List(sqlFilePath);
        if (JavaCGUtil.isCollectionEmpty(sqlList)) {
            logger.error("文件内容为空 {}", sqlFilePath);
            return null;
        }

        boolean useH2Db = confInfo.isDbUseH2();
        StringBuilder stringBuilder = new StringBuilder();
        for (String sql : sqlList) {
            if (stringBuilder.length() > 0) {
                stringBuilder.append(JACGConstants.NEW_LINE);
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
        if (sql.startsWith(JACGConstants.SQL_CREATE_TABLE_HEAD)) {
            // CREATE TABLE if not exists开头
            return sql.replace(JACGConstants.APP_NAME_IN_SQL, confInfo.getAppName());
        }

        if (sql.contains(JACGConstants.SQL_ENGINE_INNODB)) {
            // 包含ENGINE=InnoDB
            if (useH2Db) {
                return sql.replace(JACGConstants.SQL_ENGINE_INNODB, "")
                        .replace("COLLATE=utf8_bin", "");
            }
            return sql;
        }

        String trimSql = sql.trim();
        if (StringUtils.startsWithAny(trimSql, "PRIMARY KEY", "INDEX", "UNIQUE INDEX")) {
            // PRIMARY KEY、INDEX、UNIQUE INDEX开头
            if (useH2Db) {
                return sql.replace(JACGConstants.APP_NAME_IN_SQL, confInfo.getAppName())
                        .replaceAll("\\([0-9]+\\)", "");
            }
            return sql.replace(JACGConstants.APP_NAME_IN_SQL, confInfo.getAppName());
        }

        // 其他情况
        if (useH2Db) {
            return sql.replace(" text ", " varchar(65536) ");
        }
        return sql;
    }

    // 清理数据库表
    private boolean truncateTables() {
        logger.info("清理数据库表");
        for (DbTableInfoEnum dbTableInfoEnum : DbTableInfoEnum.values()) {
            if (!dbOperator.truncateTable(dbTableInfoEnum.getTableName(dbOperWrapper.getAppName()))) {
                return false;
            }
        }
        return true;
    }

    private void initWriteDbHandler(AbstractWriteDbHandler<?> writeDbHandler) {
        writeDbHandlerMap.put(writeDbHandler.getCurrentSimpleClassName(), writeDbHandler);

        writeDbHandler.setDbOperWrapper(dbOperWrapper);
        writeDbHandler.setDbOperator(dbOperator);
        writeDbHandler.setBatchSize(confInfo.getDbInsertBatchSize());
        writeDbHandler.setAllowedClassPrefixSet(allowedClassPrefixSet);
        writeDbHandler.setThreadPoolExecutor(threadPoolExecutor);
        writeDbHandler.setTaskQueueMaxSize(taskQueueMaxSize);
        writeDbHandler.init();
    }

    // 处理引用的类信息，需要首先处理
    private boolean handleClassName() {
        WriteDbHandler4ClassName writeDbHandler4ClassName = new WriteDbHandler4ClassName();
        initWriteDbHandler(writeDbHandler4ClassName);
        writeDbHandler4ClassName.handle(handleOutputInfo.getClassNameOutputFilePath());

        // 等待直到任务执行完毕，等待引用的类信息写入完毕，后面需要使用
        wait4TPEDone();

        if (writeDbHandler4ClassName.checkFailed()) {
            return false;
        }

        // 将类名表中的同名类更新为使用完整类名，并记录同名类
        return dbOperWrapper.updateAllSimpleName2Full();
    }

    // 处理jar包信息
    private void handleJarInfo() {
        WriteDbHandler4JarInfo writeDbHandler4JarInfo = new WriteDbHandler4JarInfo();
        initWriteDbHandler(writeDbHandler4JarInfo);
        writeDbHandler4JarInfo.handle(handleOutputInfo.getJarInfoOutputFilePath());
    }

    // 处理注解信息
    private void handleAnnotations(WriteDbHandler4MethodAnnotation writeDbHandler4MethodAnnotation) {
        // 处理类注解信息
        WriteDbHandler4ClassAnnotation writeDbHandler4ClassAnnotation = new WriteDbHandler4ClassAnnotation();
        initWriteDbHandler(writeDbHandler4ClassAnnotation);
        writeDbHandler4ClassAnnotation.handle(handleOutputInfo.getClassAnnotationOutputFilePath());

        Map<String, List<String>> classRequestMappingMap = writeDbHandler4ClassAnnotation.getClassRequestMappingMap();

        // 创建用于Spring Controller信息的类
        WriteDbHandler4SpringController writeDbHandler4SpringController = new WriteDbHandler4SpringController();
        initWriteDbHandler(writeDbHandler4SpringController);

        // 处理方法注解信息，需要在类注解之后处理
        initWriteDbHandler(writeDbHandler4MethodAnnotation);
        writeDbHandler4MethodAnnotation.setClassRequestMappingMap(classRequestMappingMap);
        writeDbHandler4MethodAnnotation.setWriteDbHandler4SpringController(writeDbHandler4SpringController);
        writeDbHandler4MethodAnnotation.handle(handleOutputInfo.getMethodAnnotationOutputFilePath());
    }

    // 处理方法行号
    private void handleMethodLineNumber() {
        WriteDbHandler4MethodLineNumber writeDbHandler4MethodLineNumber = new WriteDbHandler4MethodLineNumber();
        initWriteDbHandler(writeDbHandler4MethodLineNumber);
        writeDbHandler4MethodLineNumber.handle(handleOutputInfo.getMethodLineNumberOutputFilePath());
    }

    // 处理类的信息
    private void handleClassInfo() {
        WriteDbHandler4ClassInfo writeDbHandler4ClassInfo = new WriteDbHandler4ClassInfo();
        initWriteDbHandler(writeDbHandler4ClassInfo);
        writeDbHandler4ClassInfo.handle(handleOutputInfo.getClassInfoOutputFilePath());
    }

    // 处理继承与实现相关信息
    private void handleExtendsImpl() {
        // 预处理
        WriteDbHandler4ExtendsImplPre writeDbHandler4ExtendsImplPre = new WriteDbHandler4ExtendsImplPre();
        writeDbHandler4ExtendsImplPre.handle(handleOutputInfo.getExtendsImplOutputFilePath());
        Set<String> superClassOrInterfaceNameSet = writeDbHandler4ExtendsImplPre.getSuperClassOrInterfaceNameSet();

        // 正式处理
        WriteDbHandler4ExtendsImpl writeDbHandler4ExtendsImpl = new WriteDbHandler4ExtendsImpl();
        initWriteDbHandler(writeDbHandler4ExtendsImpl);
        writeDbHandler4ExtendsImpl.setSuperClassOrInterfaceNameSet(superClassOrInterfaceNameSet);
        writeDbHandler4ExtendsImpl.handle(handleOutputInfo.getExtendsImplOutputFilePath());
    }

    // 处理方法的信息
    private void handleMethodInfo() {
        WriteDbHandler4MethodInfo writeDbHandler4MethodInfo = new WriteDbHandler4MethodInfo();
        initWriteDbHandler(writeDbHandler4MethodInfo);
        writeDbHandler4MethodInfo.handle(handleOutputInfo.getMethodInfoOutputFilePath());
    }

    // 处理Lambda表达式方法信息
    private void handleLambdaMethodInfo() {
        WriteDbHandler4LambdaMethodInfo writeDbHandler4LambdaMethodInfo = new WriteDbHandler4LambdaMethodInfo();
        initWriteDbHandler(writeDbHandler4LambdaMethodInfo);
        writeDbHandler4LambdaMethodInfo.handle(handleOutputInfo.getLambdaMethodInfoOutputFilePath());
    }

    // 处理Spring Bean信息
    private void handleSpringBean() {
        Map<String, String> springBeanMap = new HashMap<>();
        WriteDbHandler4SpringBean writeDbHandler4SpringBean = new WriteDbHandler4SpringBean();
        initWriteDbHandler(writeDbHandler4SpringBean);
        writeDbHandler4SpringBean.setSpringBeanMap(springBeanMap);
        writeDbHandler4SpringBean.handle(handleOutputInfo.getSpringBeanOutputFilePath());

        SpringTaskCodeParser springTaskCodeParser = SpringTaskCodeParser.getLastInstance();
        if (springTaskCodeParser == null) {
            logger.error("未获取到 {} 类的实例", SpringTaskCodeParser.class.getSimpleName());
            return;
        }

        List<WriteDbData4SpringTask> springTaskList = springTaskCodeParser.getSpringTaskList();
        if (JavaCGUtil.isCollectionEmpty(springTaskList)) {
            return;
        }

        WriteDbHandler4SpringTask writeDbHandler4SpringTask = new WriteDbHandler4SpringTask();
        initWriteDbHandler(writeDbHandler4SpringTask);
        writeDbHandler4SpringTask.setSpringBeanMap(springBeanMap);
        // 写入数据库，Spring定时任务数量不会很多，可以一次写入
        writeDbHandler4SpringTask.insertDb(springTaskList);
    }

    // 类的签名中涉及继承与实现的信息1
    private void handleClassSignatureEi1() {
        WriteDbHandler4ClassSignatureEi1 writeDbHandler4ClassSignatureEi1 = new WriteDbHandler4ClassSignatureEi1();
        initWriteDbHandler(writeDbHandler4ClassSignatureEi1);
        writeDbHandler4ClassSignatureEi1.handle(handleOutputInfo.getClassSignatureEI1OutputFilePath());
    }

    // 处理方法调用关系文件
    private void handleMethodCall(WriteDbHandler4MethodAnnotation writeDbHandler4MethodAnnotation, WriteDbHandler4MethodCallInfo writeDbHandler4MethodCallInfo) {
        // 等待直到任务执行完毕，等待前面的其他文件写入数据库完毕
        wait4TPEDone();

        // 写入数据库，方法调用自定义数据
        WriteDbHandler4ExtendedData writeDbHandler4ExtendedData = new WriteDbHandler4ExtendedData();
        initWriteDbHandler(writeDbHandler4ExtendedData);

        // 写入数据库，方法调用关系相关类
        WriteDbHandler4MethodCall writeDbHandler4MethodCall = new WriteDbHandler4MethodCall();
        initWriteDbHandler(writeDbHandler4MethodCall);
        writeDbHandler4MethodCall.setWriteDbHandler4ExtendedData(writeDbHandler4ExtendedData);
        writeDbHandler4MethodCall.setSpringControllerMethodHashSet(writeDbHandler4MethodAnnotation.getSpringControllerMethodHashSet());
        writeDbHandler4MethodCall.setWithAnnotationMethodHashSet(writeDbHandler4MethodAnnotation.getWithAnnotationMethodHashSet());
        writeDbHandler4MethodCall.setWithInfoCallIdSet(writeDbHandler4MethodCallInfo.getWithInfoCallIdSet());
        writeDbHandler4MethodCall.setMethodCallAddExtList(methodCallAddExtList);
        writeDbHandler4MethodCall.setExtendedDataAddExtList(extendedDataAddExtList);
        writeDbHandler4MethodCall.handle(handleOutputInfo.getMethodCallOutputFilePath());

        // 等待直到任务执行完毕，等待方法调用关系文件写入数据库完毕
        wait4TPEDone();

        // 人工添加方法调用关系
        writeDbHandler4MethodCall.manualAddMethodCall();
    }

    // 处理方法调用信息
    private void handleMethodCallInfo(WriteDbHandler4MethodCallInfo writeDbHandler4MethodCallInfo) {
        initWriteDbHandler(writeDbHandler4MethodCallInfo);
        writeDbHandler4MethodCallInfo.handle(handleOutputInfo.getMethodCallInfoOutputFilePath());
    }

    // 显示H2数据库JDBC URL
    private void printH2JdbcUrl() {
        String h2DbFilePath = JACGFileUtil.getCanonicalPath(getH2DbFile());
        if (h2DbFilePath == null) {
            return;
        }
        String h2DbFilePathWithoutExt = JACGFileUtil.getFileNameWithOutExt(h2DbFilePath, JACGConstants.H2_FILE_EXT);
        logger.info("可用于连接H2数据库的JDBC URL:\n{}{}\n{}", JACGConstants.H2_PROTOCOL, h2DbFilePathWithoutExt, h2DbFilePath);
    }

    // 检查执行结果
    private boolean checkResult() {
        boolean success = true;

        Set<String> writeDbHandlerNameSet = writeDbHandlerMap.keySet();
        List<String> writeDbHandlerNameList = new ArrayList<>(writeDbHandlerNameSet);
        Collections.sort(writeDbHandlerNameList);

        for (String writeDbHandlerName : writeDbHandlerNameList) {
            AbstractWriteDbHandler<?> writeDbHandler = writeDbHandlerMap.get(writeDbHandlerName);
            logger.info("{} 写入数据库记录数 {}", writeDbHandlerName, writeDbHandler.getWriteRecordNum());

            if (writeDbHandler instanceof WriteDbHandler4ClassName && writeDbHandler.getWriteRecordNum() == 0) {
                logger.warn("未向数据库写入数据，请检查文件 {} ，以及配置文件指定的包名 {}", handleOutputInfo.getMethodCallOutputFilePath(),
                        OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX.getKey());
            }

            if (writeDbHandler.checkFailed()) {
                // 存在读文件写数据库失败
                success = false;
            }
        }
        return success;
    }

    // 打印重复的类名
    private void printDuplicateClasses() {
        Map<String, List<String>> duplicateClassNameMap = jCallGraph.getDuplicateClassNameMap();
        if (duplicateClassNameMap.isEmpty()) {
            logger.info("不存在重复的类名");
            return;
        }

        List<String> duplicateClassNameList = new ArrayList<>(duplicateClassNameMap.keySet());
        Collections.sort(duplicateClassNameList);

        for (String duplicateClassName : duplicateClassNameList) {
            List<String> classFilePathList = duplicateClassNameMap.get(duplicateClassName);
            logger.info("重复的类名 {} 使用的class文件 {}", duplicateClassName, classFilePathList.get(0));
            for (int i = 1; i < classFilePathList.size(); i++) {
                logger.info("重复的类名 {} 跳过的class文件 {}", duplicateClassName, classFilePathList.get(i));
            }
        }
    }
}

