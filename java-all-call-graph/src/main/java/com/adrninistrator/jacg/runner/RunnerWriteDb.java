package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.DbConfInfo;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.manualaddmethodcall.AbstractManualAddMethodCall1;
import com.adrninistrator.jacg.handler.fieldrelationship.MethodCallPassedFieldRelationshipHandler;
import com.adrninistrator.jacg.handler.mybatis.MyBatisMSJavaColumnHandler;
import com.adrninistrator.jacg.handler.writedb.AbstractWriteDbHandler;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassAnnotation;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassName;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassSigExtImplGenerics;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassSignatureEi1;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ClassSignatureGenerics;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ExtendsImpl;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4ExtendsImplPre;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4FieldAnnotation;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4FieldGenericsType;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4FieldInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4FieldRelationship;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4GetMethod;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4InnerClassInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4JarInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4LambdaMethodInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodAnnotation;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodArgAnnotation;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodArgGenericsType;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodArgument;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCall;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCallInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCallMethodCallReturn;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCallStaticField;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodCatch;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodFinally;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodLineNumber;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodReturnArgSeq;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodReturnCallId;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodReturnGenericsType;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MethodThrow;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSSelectColumn;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSSetColumn;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSTable;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSWhereColumn;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MyBatisMSWriteTable;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MybatisMSColumn;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MybatisMSEntity;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4MybatisMSGetSetDb;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4PropertiesConf;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SetMethod;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SetMethodAssignInfo;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SfFieldMethodCall;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringBean;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringController;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringTaskAnnotation;
import com.adrninistrator.jacg.handler.writedb.WriteDbHandler4SpringTaskXml;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
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
    private final WriteDbResult writeDbResult = new WriteDbResult();

    // jar包及允许处理的类名或包名前缀没有变化时是否跳过写数据库操作
    private boolean skipWhenNotModified = false;

    // 需要处理的包名/类名前缀
    private Set<String> allowedClassPrefixSet;

    // 人工添加方法调用关系类列表
    private List<AbstractManualAddMethodCall1> manualAddMethodCall1List;

    // 是否使用H2数据库
    private boolean useH2Db;

    // 写数据库次数
    private int writeDbTimes = 0;

    public RunnerWriteDb() {
        super();
    }

    public RunnerWriteDb(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    @Override
    public boolean preHandle() {
        // 读取其他配置文件
        allowedClassPrefixSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX, true);
        if (allowedClassPrefixSet.isEmpty()) {
            logger.info("所有包中的class文件都需要处理");
        } else {
            logger.info("仅处理以下包中的class文件\n{}", StringUtils.join(allowedClassPrefixSet, "\n"));
        }

        DbConfInfo dbConfInfo = dbOperator.getDbConfInfo();
        // 是否使用H2数据库
        useH2Db = dbConfInfo.isUseH2Db();
        if (!useH2Db && JACGSqlUtil.isMySQLDb(dbConfInfo.getDriverClassName())) {
            if (!dbConfInfo.getDbUrl().contains(JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS)) {
                logger.error("使用MYSQL时，请在{}参数指定{}", ConfigDbKeyEnum.CDKE_DB_URL.getKey(), JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS);
                return false;
            }
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
            logger.error("H2数据库文件不是文件 {}", JavaCGFileUtil.getCanonicalPath(h2DbFile));
            return false;
        }

        // 检查H2数据库文件是否可写
        return checkH2DbFileWritable(h2DbFile);
    }

    // 执行实际处理
    private boolean operate() {
        List<String> jarPathList = null;

        // 判断jar包及允许处理的类名或包名前缀没有变化时是否跳过写数据库操作
        if (skipWhenNotModified) {
            // 获得需要处理的jar包列表
            jarPathList = getJarPathList();
            /*
                检查配置文件中指定的jar包是否都在jar_info表中且未发生变化（用于判断是否可以跳过写数据库步骤）
                检查允许处理的类名或包名前缀是否有变化
             */
            if (checkAllJarExistsNotModified(jarPathList) && !checkAllowedClassPrefixModified()) {
                logger.info("有通过参数指定，且jar包及允许处理的类名或包名前缀没有变化，跳过写数据库操作");
                return true;
            }
        }

        logger.info("后续会尝试执行写数据库操作");
        writeDbTimes++;

        // 创建数据库表
        if (!createTables()) {
            return false;
        }

        // 清理数据库表
        if (!truncateTables()) {
            return false;
        }

        // 添加用于人工添加方法调用关系的处理类
        if (!addManualAddMethodCallExtensions()) {
            return false;
        }

        // 在数据库中写入允许处理的类名前缀
        if (!writeAllowedClassPrefix()) {
            return false;
        }

        // 调用java-callgraph2生成jar包的方法调用关系
        if (!callJavaCallGraph2(jarPathList)) {
            return false;
        }

        // 创建线程，参数固定指定为10，即使用10个线程
        createThreadPoolExecutor(10);

        // 处理引用的类信息，需要首先处理
        if (!handleClassName()) {
            return false;
        }

        // 处理jar包信息
        if (!handleJarInfo()) {
            return false;
        }

        JavaCGCounter springTaskAnnotationCounter = new JavaCGCounter();
        // 处理Spring相关信息
        if (!handleSpringInfo(springTaskAnnotationCounter)) {
            return false;
        }

        Set<String> withAnnotationMethodHashSet = new HashSet<>();
        // 处理注解信息
        if (!handleAnnotations(withAnnotationMethodHashSet, springTaskAnnotationCounter)) {
            return false;
        }

        Set<String> withArgsGenericsTypeMethodHash = new HashSet<>();
        Set<String> withReturnGenericsTypeMethodHash = new HashSet<>();
        Set<Integer> withInfoCallIdSet = new HashSet<>();
        // 处理方法
        if (!handleMethod(withArgsGenericsTypeMethodHash, withReturnGenericsTypeMethodHash, withInfoCallIdSet)) {
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

        Set<String> myBatisMapperSimpleNameSet = new HashSet<>();
        Set<String> myBatisMapperMethodWriteSet = new HashSet<>();
        // 处理MyBatis信息
        if (!handleMyBatisInfo(myBatisMapperSimpleNameSet, myBatisMapperMethodWriteSet)) {
            return false;
        }

        // 处理类的签名中涉及继承与实现的信息1
        if (!handleClassSignatureEi1()) {
            return false;
        }

        /*
            get方法对应的信息
            key
                唯一类名
            value
                get方法名称Set
         */
        Map<String, Set<String>> getMethodSimpleClassMap = new HashMap<>();

        /*
            set方法对应的信息
            key
                唯一类名
            value
                set方法名称Set
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

        // 处理方法调用关系文件（需要在后面执行）
        if (!handleMethodCall(extendsSimpleClassNameMap, withAnnotationMethodHashSet, withInfoCallIdSet, withArgsGenericsTypeMethodHash, withReturnGenericsTypeMethodHash,
                myBatisMapperSimpleNameSet, myBatisMapperMethodWriteSet, getMethodSimpleClassMap, setMethodSimpleClassMap)) {
            return false;
        }

        // 处理通过方法调用传递的get/set方法关联关系（需要在处理方法调用关系后面执行）
        if (!handleMethodCallPassedGetSet()) {
            return false;
        }

        // 处理MyBatis XML文件中sql脚本的字段与Java代码的关联关系（需要在处理方法调用关系后面执行）
        handleMyBatisMSJavaColumn();

        // 人工添加方法调用关系（需要在方法调用关系文件处理完毕后执行）
        if (!manualAddMethodCall()) {
            return false;
        }

        // 检查执行结果
        if (!checkResult()) {
            return false;
        }

        // 打印重复的类名
        printDuplicateClasses();

        if (useH2Db) {
            // 显示H2数据库JDBC URL
            printH2JdbcUrl();
        }
        return true;
    }

    // 添加用于根据方法调用信息添加方法调用关系的处理类
    private boolean addManualAddMethodCallExtensions() {
        List<String> manualAddMethodCallClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1, true);
        if (JavaCGUtil.isCollectionEmpty(manualAddMethodCallClassList)) {
            logger.info("未指定用于人工添加方法调用关系的处理类，跳过 {}", OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1.getKey());
            manualAddMethodCall1List = Collections.emptyList();
            return true;
        }

        logger.info("指定用于人工添加方法调用关系的处理类\n{}", StringUtils.join(manualAddMethodCallClassList, "\n"));
        manualAddMethodCall1List = new ArrayList<>(manualAddMethodCallClassList.size());
        try {
            for (String manualAddMethodCallClassName : manualAddMethodCallClassList) {
                AbstractManualAddMethodCall1 manualAddMethodCall1 = JACGUtil.genClassObject(manualAddMethodCallClassName, AbstractManualAddMethodCall1.class,
                        new Class<?>[]{DbOperator.class, DbOperWrapper.class}, new Object[]{dbOperator, dbOperWrapper});
                if (manualAddMethodCall1 == null) {
                    return false;
                }
                manualAddMethodCall1List.add(manualAddMethodCall1);
            }
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
        return true;
    }

    // 在数据库中写入允许处理的类名前缀
    private boolean writeAllowedClassPrefix() {
        if (allowedClassPrefixSet.isEmpty()) {
            return true;
        }

        // 生成用于插入数据的sql语句
        String sql = dbOperWrapper.genAndCacheInsertSql(DbTableInfoEnum.DTIE_ALLOWED_CLASS_PREFIX, DbInsertMode.DIME_INSERT);
        List<String> allowedClassPrefixList = new ArrayList<>(allowedClassPrefixSet);
        Collections.sort(allowedClassPrefixList);
        for (int i = 0; i < allowedClassPrefixList.size(); i++) {
            if (!dbOperator.insert(sql, i, allowedClassPrefixList.get(i))) {
                return false;
            }
        }
        return true;
    }

    /**
     * 检查配置文件中指定的jar包是否都在jar_info表中且未发生变化（用于判断是否可以跳过写数据库步骤）
     *
     * @param jarPathList
     * @return true: jar包未发生变化 false: jar包有发生变化
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
    private boolean createTables() {
        logger.info("创建数据库表");
        for (DbTableInfoEnum dbTableInfoEnum : DbTableInfoEnum.values()) {
            if (DbTableInfoEnum.DTIE_ILLEGAL == dbTableInfoEnum) {
                continue;
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
        String sqlFilePath = JACGUtil.getInputRootPath() + InputDirEnum.IDE_SQL.getDirName() + "/" + sqlFileName;
        List<String> sqlList = JavaCGFileUtil.readFile2List(sqlFilePath);
        if (JavaCGUtil.isCollectionEmpty(sqlList)) {
            logger.error("文件内容为空 {}", sqlFilePath);
            return null;
        }

        StringBuilder stringBuilder = new StringBuilder();
        for (String sql : sqlList) {
            if (stringBuilder.length() > 0) {
                stringBuilder.append(JavaCGConstants.NEW_LINE);
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
            return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
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
                return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix)
                        .replaceAll("\\([0-9]+\\)", "");
            }
            return JACGSqlUtil.replaceFlagInSql(sql, appName, tableSuffix);
        }

        // 其他情况
        if (useH2Db) {
            return sql.replace(" text ", " varchar(65536) ")
                    .replace("COLLATE utf8mb4_bin", "");
        }
        return sql;
    }

    // 清理数据库表
    private boolean truncateTables() {
        logger.info("清理数据库表");
        for (DbTableInfoEnum dbTableInfoEnum : DbTableInfoEnum.values()) {
            if (DbTableInfoEnum.DTIE_ILLEGAL != dbTableInfoEnum &&
                    !dbOperator.truncateTable(dbTableInfoEnum.getTableName())) {
                return false;
            }
        }
        return true;
    }

    // 初始化用于写入数据库的类
    private void initWriteDbHandler(AbstractWriteDbHandler<?> writeDbHandler) {
        writeDbHandler.setDbOperWrapper(dbOperWrapper);
        writeDbHandler.setDbOperator(dbOperator);
        writeDbHandler.setDbInsertBatchSize(dbOperator.getDbInsertBatchSize());
        writeDbHandler.setAllowedClassPrefixSet(allowedClassPrefixSet);
        writeDbHandler.setThreadPoolExecutor(threadPoolExecutor);
        writeDbHandler.setTaskQueueMaxSize(taskQueueMaxSize);
        writeDbHandler.init();
    }

    // 处理引用的类信息，需要首先处理
    private boolean handleClassName() {
        WriteDbHandler4ClassName writeDbHandler4ClassName = new WriteDbHandler4ClassName(writeDbResult);
        initWriteDbHandler(writeDbHandler4ClassName);
        if (!writeDbHandler4ClassName.handle(javaCGOutputInfo)) {
            return false;
        }

        // 等待直到任务执行完毕，等待引用的类信息写入完毕，后面需要使用
        wait4TPEDone();

        if (writeDbHandler4ClassName.checkFailed()) {
            return false;
        }

        // 将类名表中的同名类更新为使用完整类名，并记录同名类
        return dbOperWrapper.updateAllSimpleName2Full();
    }

    // 处理jar包信息
    private boolean handleJarInfo() {
        WriteDbHandler4JarInfo writeDbHandler4JarInfo = new WriteDbHandler4JarInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4JarInfo);
        return writeDbHandler4JarInfo.handle(javaCGOutputInfo);
    }

    // 处理注解信息
    private boolean handleAnnotations(Set<String> withAnnotationMethodHashSet, JavaCGCounter springTaskAnnotationCounter) {
        WriteDbHandler4MethodAnnotation writeDbHandler4MethodAnnotation = new WriteDbHandler4MethodAnnotation(writeDbResult);
        // 处理类注解信息
        WriteDbHandler4ClassAnnotation writeDbHandler4ClassAnnotation = new WriteDbHandler4ClassAnnotation(writeDbResult);
        initWriteDbHandler(writeDbHandler4ClassAnnotation);
        if (!writeDbHandler4ClassAnnotation.handle(javaCGOutputInfo)) {
            return false;
        }

        Map<String, List<String>> classRequestMappingMap = writeDbHandler4ClassAnnotation.getClassRequestMappingMap();

        // 创建处理Spring Controller信息的类
        WriteDbHandler4SpringController writeDbHandler4SpringController = new WriteDbHandler4SpringController(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringController);

        // 创建处理通过注解定义的Spring Task信息的类
        WriteDbHandler4SpringTaskAnnotation writeDbHandler4SpringTaskAnnotation = new WriteDbHandler4SpringTaskAnnotation(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringTaskAnnotation);
        // 由于spring_task会分别写入通过XML及注解定义的信息，因此这里需要修改recordId的值
        writeDbHandler4SpringTaskAnnotation.setRecordId(springTaskAnnotationCounter.getCount());

        // 处理方法注解信息，需要在类注解之后处理
        initWriteDbHandler(writeDbHandler4MethodAnnotation);
        writeDbHandler4MethodAnnotation.setClassRequestMappingMap(classRequestMappingMap);
        writeDbHandler4MethodAnnotation.setWriteDbHandler4SpringController(writeDbHandler4SpringController);
        writeDbHandler4MethodAnnotation.setWriteDbHandler4SpringTaskAnnotation(writeDbHandler4SpringTaskAnnotation);
        writeDbHandler4MethodAnnotation.setWithAnnotationMethodHashSet(withAnnotationMethodHashSet);
        if (!writeDbHandler4MethodAnnotation.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理方法参数注解信息
        WriteDbHandler4MethodArgAnnotation writeDbHandler4MethodArgAnnotation = new WriteDbHandler4MethodArgAnnotation(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodArgAnnotation);
        return writeDbHandler4MethodArgAnnotation.handle(javaCGOutputInfo);
    }

    // 处理方法
    private boolean handleMethod(Set<String> withArgsGenericsTypeMethodHash, Set<String> withReturnGenericsTypeMethodHash, Set<Integer> withInfoCallIdSet) {
        // 处理方法行号
        WriteDbHandler4MethodLineNumber writeDbHandler4MethodLineNumber = new WriteDbHandler4MethodLineNumber(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodLineNumber);
        if (!writeDbHandler4MethodLineNumber.handle(javaCGOutputInfo)) {
            return false;
        }

        WriteDbHandler4MethodInfo writeDbHandler4MethodInfo = new WriteDbHandler4MethodInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodInfo);
        if (!writeDbHandler4MethodInfo.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理方法参数
        WriteDbHandler4MethodArgument writeDbHandler4MethodArgument = new WriteDbHandler4MethodArgument(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodArgument);
        if (!writeDbHandler4MethodArgument.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理方法参数泛型类型
        WriteDbHandler4MethodArgGenericsType writeDbHandler4MethodArgGenericsType = new WriteDbHandler4MethodArgGenericsType(writeDbResult);
        writeDbHandler4MethodArgGenericsType.setWithArgsGenericsTypeMethodHash(withArgsGenericsTypeMethodHash);
        initWriteDbHandler(writeDbHandler4MethodArgGenericsType);
        if (!writeDbHandler4MethodArgGenericsType.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理方法返回泛型类型
        WriteDbHandler4MethodReturnGenericsType writeDbHandler4MethodReturnGenericsType = new WriteDbHandler4MethodReturnGenericsType(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodReturnGenericsType);
        writeDbHandler4MethodReturnGenericsType.setWithReturnGenericsTypeMethodHash(withReturnGenericsTypeMethodHash);
        if (!writeDbHandler4MethodReturnGenericsType.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理方法调用信息（需要在处理方法调用关系文件之前完成）
        WriteDbHandler4MethodCallInfo writeDbHandler4MethodCallInfo = new WriteDbHandler4MethodCallInfo(writeDbResult);
        writeDbHandler4MethodCallInfo.setWithInfoCallIdSet(withInfoCallIdSet);
        initWriteDbHandler(writeDbHandler4MethodCallInfo);
        if (!writeDbHandler4MethodCallInfo.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理方法调用使用方法调用返回值信息
        WriteDbHandler4MethodCallMethodCallReturn writeDbHandler4MethodCallMethodCallReturn = new WriteDbHandler4MethodCallMethodCallReturn(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodCallMethodCallReturn);
        if (!writeDbHandler4MethodCallMethodCallReturn.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理方法调用使用静态字段信息
        WriteDbHandler4MethodCallStaticField writeDbHandler4MethodCallStaticField = new WriteDbHandler4MethodCallStaticField(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodCallStaticField);
        if (!writeDbHandler4MethodCallStaticField.handle(javaCGOutputInfo)) {
            return false;
        }

        // 方法返回值对应的方法参数序号
        WriteDbHandler4MethodReturnArgSeq writeDbHandler4MethodReturnArgSeq = new WriteDbHandler4MethodReturnArgSeq(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodReturnArgSeq);
        if (!writeDbHandler4MethodReturnArgSeq.handle(javaCGOutputInfo)) {
            return false;
        }

        // 方法返回值对应的方法调用序号
        WriteDbHandler4MethodReturnCallId writeDbHandler4MethodReturnCallId = new WriteDbHandler4MethodReturnCallId(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodReturnCallId);
        if (!writeDbHandler4MethodReturnCallId.handle(javaCGOutputInfo)) {
            return false;
        }

        // 方法的catch信息
        WriteDbHandler4MethodCatch writeDbHandler4MethodTryCatchFinally = new WriteDbHandler4MethodCatch(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodTryCatchFinally);
        if (!writeDbHandler4MethodTryCatchFinally.handle(javaCGOutputInfo)) {
            return false;
        }

        // 方法的finally信息
        WriteDbHandler4MethodFinally writeDbHandler4MethodFinally = new WriteDbHandler4MethodFinally(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodFinally);
        if (!writeDbHandler4MethodFinally.handle(javaCGOutputInfo)) {
            return false;
        }

        // 方法的throw信息
        WriteDbHandler4MethodThrow writeDbHandler4MethodThrow = new WriteDbHandler4MethodThrow(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodThrow);
        return writeDbHandler4MethodThrow.handle(javaCGOutputInfo);
    }

    // 处理类的信息
    private boolean handleClassInfo(Set<String> enumSimpleClassNameSet) {
        WriteDbHandler4ClassInfo writeDbHandler4ClassInfo = new WriteDbHandler4ClassInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4ClassInfo);
        writeDbHandler4ClassInfo.setEnumSimpleClassNameSet(enumSimpleClassNameSet);
        if (!writeDbHandler4ClassInfo.handle(javaCGOutputInfo)) {
            return false;
        }

        WriteDbHandler4InnerClassInfo writeDbHandler4InnerClassInfo = new WriteDbHandler4InnerClassInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4InnerClassInfo);
        return writeDbHandler4InnerClassInfo.handle(javaCGOutputInfo);
    }

    // 处理继承与实现相关信息
    private boolean handleExtendsImpl(Map<String, String> extendsSimpleClassNameMap) {
        // 预处理
        WriteDbHandler4ExtendsImplPre writeDbHandler4ExtendsImplPre = new WriteDbHandler4ExtendsImplPre(writeDbResult);
        writeDbHandler4ExtendsImplPre.init();
        writeDbHandler4ExtendsImplPre.handle(javaCGOutputInfo);
        Set<String> superClassOrInterfaceNameSet = writeDbHandler4ExtendsImplPre.getSuperClassOrInterfaceNameSet();

        // 正式处理
        WriteDbHandler4ExtendsImpl writeDbHandler4ExtendsImpl = new WriteDbHandler4ExtendsImpl(writeDbResult);
        initWriteDbHandler(writeDbHandler4ExtendsImpl);
        writeDbHandler4ExtendsImpl.setSuperClassOrInterfaceNameSet(superClassOrInterfaceNameSet);
        writeDbHandler4ExtendsImpl.setExtendsSimpleClassNameMap(extendsSimpleClassNameMap);
        return writeDbHandler4ExtendsImpl.handle(javaCGOutputInfo);
    }

    // 处理Lambda表达式方法信息
    private boolean handleLambdaMethodInfo() {
        WriteDbHandler4LambdaMethodInfo writeDbHandler4LambdaMethodInfo = new WriteDbHandler4LambdaMethodInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4LambdaMethodInfo);
        return writeDbHandler4LambdaMethodInfo.handle(javaCGOutputInfo);
    }

    // 处理Spring相关信息
    private boolean handleSpringInfo(JavaCGCounter springTaskAnnotationCounter) {
        // 处理Spring Bean信息
        Map<String, String> springBeanMap = new HashMap<>();
        WriteDbHandler4SpringBean writeDbHandler4SpringBean = new WriteDbHandler4SpringBean(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringBean);
        writeDbHandler4SpringBean.setSpringBeanMap(springBeanMap);
        if (!writeDbHandler4SpringBean.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理Spring Task信息
        WriteDbHandler4SpringTaskXml writeDbHandler4SpringTask = new WriteDbHandler4SpringTaskXml(writeDbResult);
        initWriteDbHandler(writeDbHandler4SpringTask);
        writeDbHandler4SpringTask.setSpringBeanMap(springBeanMap);
        if (!writeDbHandler4SpringTask.handle(javaCGOutputInfo)) {
            return false;
        }
        springTaskAnnotationCounter.setCount(writeDbHandler4SpringTask.getRecordId());
        return true;
    }

    // 处理MyBatis信息
    private boolean handleMyBatisInfo(Set<String> myBatisMapperSimpleNameSet, Set<String> myBatisMapperMethodWriteSet) {
        // 处理MyBatis Mapper方法操作的数据库表信息
        WriteDbHandler4MyBatisMSTable writeDbHandler4MyBatisMSTable = new WriteDbHandler4MyBatisMSTable(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSTable);
        writeDbHandler4MyBatisMSTable.setMyBatisMapperSimpleNameSet(myBatisMapperSimpleNameSet);
        if (!writeDbHandler4MyBatisMSTable.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理MyBatis Mapper方法写的数据库表信息
        WriteDbHandler4MyBatisMSWriteTable writeDbHandler4MyBatisMSWriteTable = new WriteDbHandler4MyBatisMSWriteTable(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSWriteTable);
        writeDbHandler4MyBatisMSWriteTable.setMyBatisMapperMethodWriteSet(myBatisMapperMethodWriteSet);
        if (!writeDbHandler4MyBatisMSWriteTable.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理MyBatis的Entity与数据库字段名信息
        WriteDbHandler4MybatisMSColumn writeDbHandler4MybatisMSColumn = new WriteDbHandler4MybatisMSColumn(writeDbResult);
        initWriteDbHandler(writeDbHandler4MybatisMSColumn);
        if (!writeDbHandler4MybatisMSColumn.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理MyBatis的Entity与Mapper、表名
        WriteDbHandler4MybatisMSEntity writeDbHandler4MybatisMSEntity = new WriteDbHandler4MybatisMSEntity(writeDbResult);
        initWriteDbHandler(writeDbHandler4MybatisMSEntity);
        if (!writeDbHandler4MybatisMSEntity.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理MyBatis的update set子句的字段信息
        WriteDbHandler4MyBatisMSSetColumn writeDbHandler4MyBatisMSSetColumn = new WriteDbHandler4MyBatisMSSetColumn(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSSetColumn);
        if (!writeDbHandler4MyBatisMSSetColumn.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理MyBatis的where子句的字段信息
        WriteDbHandler4MyBatisMSWhereColumn writeDbHandler4MyBatisMSWhereColumn = new WriteDbHandler4MyBatisMSWhereColumn(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSWhereColumn);
        if (!writeDbHandler4MyBatisMSWhereColumn.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理MyBatis的select的字段信息
        WriteDbHandler4MyBatisMSSelectColumn writeDbHandler4MyBatisMSSelectColumn = new WriteDbHandler4MyBatisMSSelectColumn(writeDbResult);
        initWriteDbHandler(writeDbHandler4MyBatisMSSelectColumn);
        return writeDbHandler4MyBatisMSSelectColumn.handle(javaCGOutputInfo);
    }

    // 处理类的签名中涉及继承与实现的信息1
    private boolean handleClassSignatureEi1() {
        WriteDbHandler4ClassSignatureEi1 writeDbHandler4ClassSignatureEi1 = new WriteDbHandler4ClassSignatureEi1(writeDbResult);
        initWriteDbHandler(writeDbHandler4ClassSignatureEi1);
        if (!writeDbHandler4ClassSignatureEi1.handle(javaCGOutputInfo)) {
            return false;
        }

        WriteDbHandler4ClassSignatureGenerics writeDbHandler4ClassSignatureGenerics = new WriteDbHandler4ClassSignatureGenerics(writeDbResult);
        initWriteDbHandler(writeDbHandler4ClassSignatureGenerics);
        if (!writeDbHandler4ClassSignatureGenerics.handle(javaCGOutputInfo)) {
            return false;
        }

        WriteDbHandler4ClassSigExtImplGenerics writeDbHandler4ClassSigExtImplGenerics = new WriteDbHandler4ClassSigExtImplGenerics(writeDbResult);
        initWriteDbHandler(writeDbHandler4ClassSigExtImplGenerics);
        return writeDbHandler4ClassSigExtImplGenerics.handle(javaCGOutputInfo);
    }

    // 处理get/set方法，以对应的调用关系
    private boolean handleFieldRelationship(Map<String, String> extendsSimpleClassNameMap, Set<String> enumSimpleClassNameSet, Map<String, Set<String>> getMethodSimpleClassMap,
                                            Map<String, Set<String>> setMethodSimpleClassMap) {

        WriteDbHandler4GetMethod writeDbHandler4GetMethod = new WriteDbHandler4GetMethod(writeDbResult);
        initWriteDbHandler(writeDbHandler4GetMethod);
        writeDbHandler4GetMethod.setGetMethodSimpleClassMap(getMethodSimpleClassMap);
        if (!writeDbHandler4GetMethod.handle(javaCGOutputInfo)) {
            return false;
        }

        WriteDbHandler4SetMethod writeDbHandler4SetMethod = new WriteDbHandler4SetMethod(writeDbResult);
        initWriteDbHandler(writeDbHandler4SetMethod);
        writeDbHandler4SetMethod.setSetMethodSimpleClassMap(setMethodSimpleClassMap);
        if (!writeDbHandler4SetMethod.handle(javaCGOutputInfo)) {
            return false;
        }

        WriteDbHandler4FieldRelationship writeDbHandler4FieldRelationship = new WriteDbHandler4FieldRelationship(writeDbResult);
        initWriteDbHandler(writeDbHandler4FieldRelationship);
        writeDbHandler4FieldRelationship.setGetMethodSimpleClassMap(getMethodSimpleClassMap);
        writeDbHandler4FieldRelationship.setSetMethodSimpleClassMap(setMethodSimpleClassMap);
        writeDbHandler4FieldRelationship.setExtendsSimpleClassNameMap(extendsSimpleClassNameMap);
        writeDbHandler4FieldRelationship.setEnumSimpleClassNameSet(enumSimpleClassNameSet);
        if (!writeDbHandler4FieldRelationship.handle(javaCGOutputInfo)) {
            return false;
        }

        WriteDbHandler4FieldGenericsType writeDbHandler4FieldGenericsType = new WriteDbHandler4FieldGenericsType(writeDbResult);
        initWriteDbHandler(writeDbHandler4FieldGenericsType);
        return writeDbHandler4FieldGenericsType.handle(javaCGOutputInfo);
    }

    // 处理配置文件
    private boolean handleConfFile() {
        WriteDbHandler4PropertiesConf writeDbHandler4PropertiesConf = new WriteDbHandler4PropertiesConf(writeDbResult);
        initWriteDbHandler(writeDbHandler4PropertiesConf);
        return writeDbHandler4PropertiesConf.handle(javaCGOutputInfo);
    }

    // 处理通过方法调用传递的get/set方法关联关系（需要在处理方法调用关系后面执行）
    private boolean handleMethodCallPassedGetSet() {
        if (!Boolean.TRUE.equals(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_HANDLE_GET_SET_FIELD_RELATIONSHIP))) {
            logger.info("不处理通过get/set方法关联的字段关联关系");
            return true;
        }

        WriteDbHandler4SetMethodAssignInfo writeDbHandler4SetMethodAssignInfo = new WriteDbHandler4SetMethodAssignInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4SetMethodAssignInfo);

        MethodCallPassedFieldRelationshipHandler methodCallPassedFieldRelationshipHandler = new MethodCallPassedFieldRelationshipHandler(dbOperWrapper);
        methodCallPassedFieldRelationshipHandler.setWriteDbHandler4SetMethodAssignInfo(writeDbHandler4SetMethodAssignInfo);
        if (!methodCallPassedFieldRelationshipHandler.handle(javaCGOutputInfo.getOutputDirPath())) {
            return false;
        }

        // 等待写入数据库完成
        wait4TPEDone();
        return true;
    }

    // 处理MyBatis XML文件中sql脚本的字段与Java代码的关联关系（使用MySQL）
    private void handleMyBatisMSJavaColumn() {
        WriteDbHandler4MybatisMSGetSetDb writeDbHandler4MybatisMsMapperArgDb = new WriteDbHandler4MybatisMSGetSetDb(writeDbResult);
        initWriteDbHandler(writeDbHandler4MybatisMsMapperArgDb);

        MyBatisMSJavaColumnHandler myBatisMSJavaColumnHandler = new MyBatisMSJavaColumnHandler(dbOperWrapper);
        myBatisMSJavaColumnHandler.setWriteDbHandler4MybatisMSMapperArgDb(writeDbHandler4MybatisMsMapperArgDb);
        myBatisMSJavaColumnHandler.handle(javaCGOutputInfo.getOutputDirPath());

        // 等待写入数据库完成
        wait4TPEDone();
    }

    // 处理方法调用关系文件
    private boolean handleMethodCall(Map<String, String> extendsSimpleClassNameMap,
                                     Set<String> withAnnotationMethodHashSet,
                                     Set<Integer> withInfoCallIdSet,
                                     Set<String> withArgsGenericsTypeMethodHashSet,
                                     Set<String> withReturnGenericsTypeMethodHashSet,
                                     Set<String> myBatisMapperSimpleNameSet,
                                     Set<String> myBatisMapperMethodWriteSet,
                                     Map<String, Set<String>> getMethodSimpleClassMap,
                                     Map<String, Set<String>> setMethodSimpleClassMap) {
        // 等待前面的其他文件写入数据库完毕
        wait4TPEDone();

        // 写入数据库，方法调用关系相关类
        WriteDbHandler4MethodCall writeDbHandler4MethodCall = new WriteDbHandler4MethodCall(writeDbResult);
        initWriteDbHandler(writeDbHandler4MethodCall);
        writeDbHandler4MethodCall.setExtendsSimpleClassNameMap(extendsSimpleClassNameMap);
        writeDbHandler4MethodCall.setWithAnnotationMethodHashSet(withAnnotationMethodHashSet);
        writeDbHandler4MethodCall.setWithInfoCallIdSet(withInfoCallIdSet);
        writeDbHandler4MethodCall.setWithArgsGenericsTypeMethodHashSet(withArgsGenericsTypeMethodHashSet);
        writeDbHandler4MethodCall.setWithReturnGenericsTypeMethodHashSet(withReturnGenericsTypeMethodHashSet);
        writeDbHandler4MethodCall.setMyBatisMapperSimpleNameSet(myBatisMapperSimpleNameSet);
        writeDbHandler4MethodCall.setMyBatisMapperMethodWriteSet(myBatisMapperMethodWriteSet);
        writeDbHandler4MethodCall.setGetMethodSimpleClassMap(getMethodSimpleClassMap);
        writeDbHandler4MethodCall.setSetMethodSimpleClassMap(setMethodSimpleClassMap);
        if (!writeDbHandler4MethodCall.handle(javaCGOutputInfo)) {
            return false;
        }

        // 等待直到任务执行完毕，等待方法调用关系文件写入数据库完毕
        wait4TPEDone();
        return true;
    }

    // 处理字段
    boolean handleField() {
        WriteDbHandler4FieldAnnotation writeDbHandler4FieldAnnotation = new WriteDbHandler4FieldAnnotation(writeDbResult);
        initWriteDbHandler(writeDbHandler4FieldAnnotation);
        if (!writeDbHandler4FieldAnnotation.handle(javaCGOutputInfo)) {
            return false;
        }

        WriteDbHandler4FieldInfo writeDbHandler4FieldInfo = new WriteDbHandler4FieldInfo(writeDbResult);
        initWriteDbHandler(writeDbHandler4FieldInfo);
        if (!writeDbHandler4FieldInfo.handle(javaCGOutputInfo)) {
            return false;
        }

        WriteDbHandler4SfFieldMethodCall writeDbHandler4SfFieldMethodCall = new WriteDbHandler4SfFieldMethodCall(writeDbResult);
        initWriteDbHandler(writeDbHandler4SfFieldMethodCall);
        return writeDbHandler4SfFieldMethodCall.handle(javaCGOutputInfo);
    }

    // 人工添加方法调用关系（需要在方法调用关系文件处理完毕后执行）
    private boolean manualAddMethodCall() {
        for (AbstractManualAddMethodCall1 manualAddMethodCall1 : manualAddMethodCall1List) {
            if (!manualAddMethodCall1.addMethodCall()) {
                return false;
            }
        }
        return true;
    }

    // 显示H2数据库JDBC URL
    private void printH2JdbcUrl() {
        String h2DbFilePath = JavaCGFileUtil.getCanonicalPath(getH2DbFile());
        if (h2DbFilePath == null) {
            return;
        }
        String h2DbFilePathWithoutExt = JACGFileUtil.getFileNameWithOutExt(h2DbFilePath, JACGConstants.H2_FILE_EXT);
        logger.info("可用于连接H2数据库的JDBC URL:\n{}{}\n{}", JACGConstants.H2_PROTOCOL, h2DbFilePathWithoutExt, h2DbFilePath);
    }

    // 检查执行结果
    private boolean checkResult() {
        boolean success = true;

        Map<String, JavaCGCounter> writeDbNumMap = writeDbResult.getWriteDbNumMap();
        List<String> writeDbNameList = new ArrayList<>(writeDbNumMap.keySet());
        Collections.sort(writeDbNameList);
        for (String writeDbName : writeDbNameList) {
            int writeDbNum = writeDbNumMap.get(writeDbName).getCount();
            logger.info("{} 写入数据库表记录数 {}", writeDbName, writeDbNum);
            if (DbTableInfoEnum.DTIE_CLASS_NAME.getTableNameKeyword().equals(writeDbName) && writeDbNum == 0) {
                logger.warn("未向数据库写入数据，请检查方法调用文件是否为空\n{}\n以及写入数据库时需要处理的类名前缀配置 {}", javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL),
                        OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX.getKey());
            }
        }

        Map<String, JavaCGCounter> writeFileNumMap = writeDbResult.getWriteFileNumMap();
        List<String> writeFileNameList = new ArrayList<>(writeFileNumMap.keySet());
        Collections.sort(writeFileNameList);
        for (String writeFileName : writeFileNameList) {
            logger.info("{} 写入文件记录数 {}", writeFileName, writeFileNumMap.get(writeFileName).getCount());
        }

        Map<String, JavaCGCounter> failNumMap = writeDbResult.getFailNumMap();
        for (Map.Entry<String, JavaCGCounter> entry : failNumMap.entrySet()) {
            if (entry.getValue().getCount() > 0) {
                logger.error("写数据库操作失败 {}", entry.getKey());
                success = false;
            }
        }

        Map<String, Boolean> afterHandleMap = writeDbResult.getAfterHandleMap();
        for (Map.Entry<String, Boolean> entry : afterHandleMap.entrySet()) {
            if (!Boolean.TRUE.equals(entry.getValue())) {
                logger.error("在操作执行完毕前需要调用 afterHandle 方法 {}", entry.getKey());
                success = false;
            }
        }
        return success;
    }

    @Override
    protected boolean handleDb() {
        // 返回需要操作数据库
        return true;
    }

    public boolean isSkipWhenNotModified() {
        return skipWhenNotModified;
    }

    public void setSkipWhenNotModified(boolean skipWhenNotModified) {
        this.skipWhenNotModified = skipWhenNotModified;
    }

    public int getWriteDbTimes() {
        return writeDbTimes;
    }
}

