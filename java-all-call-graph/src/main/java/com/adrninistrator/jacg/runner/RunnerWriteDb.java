package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.extensions.manual_add_method_call.AbstractManualAddMethodCall1;
import com.adrninistrator.jacg.handler.method.MethodCallHandler;
import com.adrninistrator.jacg.handler.write_db.AbstractWriteDbHandler;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ClassAnnotation;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ClassInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ClassName;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ClassSignatureEi1;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ExtendsImpl;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4ExtendsImplPre;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4InnerClassInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4JarInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4LambdaMethodInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodAnnotation;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodArgGenericsType;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodArgType;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodCall;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodCallInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodInfo;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodLineNumber;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MethodReturnGenericsType;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MyBatisMSTable;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4MyBatisMSWriteTable;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4SpringBean;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4SpringController;
import com.adrninistrator.jacg.handler.write_db.WriteDbHandler4SpringTask;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
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

    // 需要处理的包名/类名前缀
    private Set<String> allowedClassPrefixSet;

    /*
        记录写入数据库的对象
        key
            写入数据库的简单类名
        value
            写入数据库的对象
     */
    private final Map<String, AbstractWriteDbHandler<?>> writeDbHandlerMap = new HashMap<>();

    // 人工添加方法调用关系类列表
    private List<AbstractManualAddMethodCall1> manualAddMethodCall1List;

    // 批量写入数据库时每次插入的数量
    private int dbInsertBatchSize;

    // 方法调用处理类
    private MethodCallHandler methodCallHandler;

    // 是否使用H2数据库
    private boolean useH2Db;

    @Override
    public boolean preHandle() {
        // 读取其他配置文件
        allowedClassPrefixSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX, true);
        if (allowedClassPrefixSet.isEmpty()) {
            logger.info("所有包中的class文件都需要处理");
        } else {
            logger.info("仅处理以下包中的class文件\n{}", StringUtils.join(allowedClassPrefixSet, "\n"));
        }

        // 是否使用H2数据库
        useH2Db = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2);
        if (!useH2Db && JACGSqlUtil.isMySQLDb(configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME))) {
            String dbUrl = configureWrapper.getMainConfig(ConfigDbKeyEnum.CDKE_DB_URL);
            if (!dbUrl.contains(JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS)) {
                logger.error("使用MYSQL时，请在{}参数指定{}", ConfigDbKeyEnum.CDKE_DB_URL.getKey(), JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS);
                return false;
            }
        }

        // 使用多线程，线程数固定为10
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_THREAD_NUM, String.valueOf(10));

        // 批量写入数据库时每次插入的数量
        dbInsertBatchSize = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE);

        // 初始化方法调用处理类
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
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
        if (!addManualAddMethodCallExtensions()) {
            return false;
        }

        // 在数据库中写入允许处理的类名前缀
        if (!writeAllowedClassPrefix()) {
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
        if (!handleJarInfo()) {
            return false;
        }

        Set<String> springControllerMethodHashSet = new HashSet<>();
        Set<String> withAnnotationMethodHashSet = new HashSet<>();
        // 处理注解信息
        handleAnnotations(springControllerMethodHashSet, withAnnotationMethodHashSet);

        Set<String> withGenericsTypeMethodHash = new HashSet<>();
        Set<Integer> withInfoCallIdSet = new HashSet<>();
        // 处理方法
        if (!handleMethod(withGenericsTypeMethodHash, withInfoCallIdSet)) {
            return false;
        }

        // 处理类的信息
        if (!handleClassInfo()) {
            return false;
        }

        // 处理继承与实现相关信息
        if (!handleExtendsImpl()) {
            return false;
        }

        // 处理Lambda表达式方法信息
        if (!handleLambdaMethodInfo()) {
            return false;
        }

        // 处理Spring Bean信息
        if (!handleSpringInfo()) {
            return false;
        }

        Set<String> myBatisMapperSet = new HashSet<>();
        Set<String> myBatisMapperMethodWriteSet = new HashSet<>();
        // 处理MyBatis信息
        if (!handleMyBatisInfo(myBatisMapperSet, myBatisMapperMethodWriteSet)) {
            return false;
        }

        // 类的签名中涉及继承与实现的信息1
        if (!handleClassSignatureEi1()) {
            return false;
        }

        // 处理方法调用关系文件（需要在后面处理）
        if (!handleMethodCall(springControllerMethodHashSet, withAnnotationMethodHashSet, withInfoCallIdSet, withGenericsTypeMethodHash, myBatisMapperSet,
                myBatisMapperMethodWriteSet)) {
            return false;
        }

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
                AbstractManualAddMethodCall1 manualAddMethodCall1 = JACGUtil.getClassObject(manualAddMethodCallClassName, AbstractManualAddMethodCall1.class);
                if (manualAddMethodCall1 == null) {
                    return false;
                }
                manualAddMethodCall1.setDbOperator(dbOperator);
                manualAddMethodCall1.setDbOperWrapper(dbOperWrapper);
                manualAddMethodCall1.setJacgExtendsImplHandler(jacgExtendsImplHandler);
                manualAddMethodCall1.setMethodCallHandler(methodCallHandler);
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
        List<String> sqlList = JACGFileUtil.readFile2List(sqlFilePath);
        if (JavaCGUtil.isCollectionEmpty(sqlList)) {
            logger.error("文件内容为空 {}", sqlFilePath);
            return null;
        }

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
            return JACGSqlUtil.replaceAppNameInSql(sql, appName);
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
                return JACGSqlUtil.replaceAppNameInSql(sql, appName)
                        .replaceAll("\\([0-9]+\\)", "");
            }
            return JACGSqlUtil.replaceAppNameInSql(sql, appName);
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

    private void initWriteDbHandler(AbstractWriteDbHandler<?> writeDbHandler) {
        writeDbHandlerMap.put(writeDbHandler.getCurrentSimpleClassName(), writeDbHandler);

        writeDbHandler.setDbOperWrapper(dbOperWrapper);
        writeDbHandler.setDbOperator(dbOperator);
        writeDbHandler.setBatchSize(dbInsertBatchSize);
        writeDbHandler.setAllowedClassPrefixSet(allowedClassPrefixSet);
        writeDbHandler.setThreadPoolExecutor(threadPoolExecutor);
        writeDbHandler.setTaskQueueMaxSize(taskQueueMaxSize);
    }

    // 处理引用的类信息，需要首先处理
    private boolean handleClassName() {
        WriteDbHandler4ClassName writeDbHandler4ClassName = new WriteDbHandler4ClassName();
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
        WriteDbHandler4JarInfo writeDbHandler4JarInfo = new WriteDbHandler4JarInfo();
        initWriteDbHandler(writeDbHandler4JarInfo);
        return writeDbHandler4JarInfo.handle(javaCGOutputInfo);
    }

    // 处理注解信息
    private boolean handleAnnotations(Set<String> springControllerMethodHashSet, Set<String> withAnnotationMethodHashSet) {
        WriteDbHandler4MethodAnnotation writeDbHandler4MethodAnnotation = new WriteDbHandler4MethodAnnotation();
        // 处理类注解信息
        WriteDbHandler4ClassAnnotation writeDbHandler4ClassAnnotation = new WriteDbHandler4ClassAnnotation();
        initWriteDbHandler(writeDbHandler4ClassAnnotation);
        if (!writeDbHandler4ClassAnnotation.handle(javaCGOutputInfo)) {
            return false;
        }

        Map<String, List<String>> classRequestMappingMap = writeDbHandler4ClassAnnotation.getClassRequestMappingMap();

        // 创建用于Spring Controller信息的类
        WriteDbHandler4SpringController writeDbHandler4SpringController = new WriteDbHandler4SpringController();
        initWriteDbHandler(writeDbHandler4SpringController);

        // 处理方法注解信息，需要在类注解之后处理
        initWriteDbHandler(writeDbHandler4MethodAnnotation);
        writeDbHandler4MethodAnnotation.setClassRequestMappingMap(classRequestMappingMap);
        writeDbHandler4MethodAnnotation.setWriteDbHandler4SpringController(writeDbHandler4SpringController);
        writeDbHandler4MethodAnnotation.setSpringControllerMethodHashSet(springControllerMethodHashSet);
        writeDbHandler4MethodAnnotation.setWithAnnotationMethodHashSet(withAnnotationMethodHashSet);
        return writeDbHandler4MethodAnnotation.handle(javaCGOutputInfo);
    }

    // 处理方法
    private boolean handleMethod(Set<String> withGenericsTypeMethodHash, Set<Integer> withInfoCallIdSet) {
        // 处理方法行号
        WriteDbHandler4MethodLineNumber writeDbHandler4MethodLineNumber = new WriteDbHandler4MethodLineNumber();
        initWriteDbHandler(writeDbHandler4MethodLineNumber);
        if (!writeDbHandler4MethodLineNumber.handle(javaCGOutputInfo)) {
            return false;
        }

        // 方法的参数类型写入数据库的类
        WriteDbHandler4MethodArgType writeDbHandler4MethodArgType = new WriteDbHandler4MethodArgType();
        initWriteDbHandler(writeDbHandler4MethodArgType);

        WriteDbHandler4MethodInfo writeDbHandler4MethodInfo = new WriteDbHandler4MethodInfo();
        initWriteDbHandler(writeDbHandler4MethodInfo);
        writeDbHandler4MethodInfo.setWriteDbHandler4MethodArgType(writeDbHandler4MethodArgType);
        if (!writeDbHandler4MethodInfo.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理方法参数泛型类型
        WriteDbHandler4MethodArgGenericsType writeDbHandler4MethodArgGenericsType = new WriteDbHandler4MethodArgGenericsType();
        writeDbHandler4MethodArgGenericsType.setWithGenericsTypeMethodHash(withGenericsTypeMethodHash);
        initWriteDbHandler(writeDbHandler4MethodArgGenericsType);
        if (!writeDbHandler4MethodArgGenericsType.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理方法返回泛型类型
        WriteDbHandler4MethodReturnGenericsType writeDbHandler4MethodReturnGenericsType = new WriteDbHandler4MethodReturnGenericsType();
        initWriteDbHandler(writeDbHandler4MethodReturnGenericsType);
        if (!writeDbHandler4MethodReturnGenericsType.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理方法调用信息（需要在处理方法调用关系文件之前完成）
        WriteDbHandler4MethodCallInfo writeDbHandler4MethodCallInfo = new WriteDbHandler4MethodCallInfo();
        writeDbHandler4MethodCallInfo.setWithInfoCallIdSet(withInfoCallIdSet);
        initWriteDbHandler(writeDbHandler4MethodCallInfo);
        return writeDbHandler4MethodCallInfo.handle(javaCGOutputInfo);
    }

    // 处理类的信息
    private boolean handleClassInfo() {
        WriteDbHandler4ClassInfo writeDbHandler4ClassInfo = new WriteDbHandler4ClassInfo();
        initWriteDbHandler(writeDbHandler4ClassInfo);
        if (!writeDbHandler4ClassInfo.handle(javaCGOutputInfo)) {
            return false;
        }

        WriteDbHandler4InnerClassInfo writeDbHandler4InnerClassInfo = new WriteDbHandler4InnerClassInfo();
        initWriteDbHandler(writeDbHandler4InnerClassInfo);
        return writeDbHandler4InnerClassInfo.handle(javaCGOutputInfo);
    }

    // 处理继承与实现相关信息
    private boolean handleExtendsImpl() {
        // 预处理
        WriteDbHandler4ExtendsImplPre writeDbHandler4ExtendsImplPre = new WriteDbHandler4ExtendsImplPre();
        writeDbHandler4ExtendsImplPre.handle(javaCGOutputInfo);
        Set<String> superClassOrInterfaceNameSet = writeDbHandler4ExtendsImplPre.getSuperClassOrInterfaceNameSet();

        // 正式处理
        WriteDbHandler4ExtendsImpl writeDbHandler4ExtendsImpl = new WriteDbHandler4ExtendsImpl();
        initWriteDbHandler(writeDbHandler4ExtendsImpl);
        writeDbHandler4ExtendsImpl.setSuperClassOrInterfaceNameSet(superClassOrInterfaceNameSet);
        return writeDbHandler4ExtendsImpl.handle(javaCGOutputInfo);
    }

    // 处理Lambda表达式方法信息
    private boolean handleLambdaMethodInfo() {
        WriteDbHandler4LambdaMethodInfo writeDbHandler4LambdaMethodInfo = new WriteDbHandler4LambdaMethodInfo();
        initWriteDbHandler(writeDbHandler4LambdaMethodInfo);
        return writeDbHandler4LambdaMethodInfo.handle(javaCGOutputInfo);
    }

    // 处理Spring相关信息
    private boolean handleSpringInfo() {
        // 处理Spring Bean信息
        Map<String, String> springBeanMap = new HashMap<>();
        WriteDbHandler4SpringBean writeDbHandler4SpringBean = new WriteDbHandler4SpringBean();
        initWriteDbHandler(writeDbHandler4SpringBean);
        writeDbHandler4SpringBean.setSpringBeanMap(springBeanMap);
        if (!writeDbHandler4SpringBean.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理Spring Task信息
        WriteDbHandler4SpringTask writeDbHandler4SpringTask = new WriteDbHandler4SpringTask();
        initWriteDbHandler(writeDbHandler4SpringTask);
        writeDbHandler4SpringTask.setSpringBeanMap(springBeanMap);
        return writeDbHandler4SpringTask.handle(javaCGOutputInfo);
    }

    // 处理MyBatis信息
    private boolean handleMyBatisInfo(Set<String> myBatisMapperSet, Set<String> myBatisMapperMethodWriteSet) {
        // 处理MyBatis数据库表信息
        WriteDbHandler4MyBatisMSTable writeDbHandler4MyBatisMSTable = new WriteDbHandler4MyBatisMSTable();
        initWriteDbHandler(writeDbHandler4MyBatisMSTable);
        writeDbHandler4MyBatisMSTable.setMyBatisMapperSet(myBatisMapperSet);
        if (!writeDbHandler4MyBatisMSTable.handle(javaCGOutputInfo)) {
            return false;
        }

        // 处理MyBatis写数据库表信息
        WriteDbHandler4MyBatisMSWriteTable writeDbHandler4MyBatisMSWriteTable = new WriteDbHandler4MyBatisMSWriteTable();
        initWriteDbHandler(writeDbHandler4MyBatisMSWriteTable);
        writeDbHandler4MyBatisMSWriteTable.setMyBatisMapperMethodWriteSet(myBatisMapperMethodWriteSet);
        return writeDbHandler4MyBatisMSWriteTable.handle(javaCGOutputInfo);
    }

    // 类的签名中涉及继承与实现的信息1
    private boolean handleClassSignatureEi1() {
        WriteDbHandler4ClassSignatureEi1 writeDbHandler4ClassSignatureEi1 = new WriteDbHandler4ClassSignatureEi1();
        initWriteDbHandler(writeDbHandler4ClassSignatureEi1);
        return writeDbHandler4ClassSignatureEi1.handle(javaCGOutputInfo);
    }

    // 处理方法调用关系文件
    private boolean handleMethodCall(Set<String> springControllerMethodHashSet,
                                     Set<String> withAnnotationMethodHashSet,
                                     Set<Integer> withInfoCallIdSet,
                                     Set<String> withGenericsTypeMethodHash,
                                     Set<String> myBatisMapperSet,
                                     Set<String> myBatisMapperMethodWriteSet) {
        // 等待直到任务执行完毕，等待前面的其他文件写入数据库完毕
        wait4TPEDone();

        // 写入数据库，方法调用关系相关类
        WriteDbHandler4MethodCall writeDbHandler4MethodCall = new WriteDbHandler4MethodCall();
        initWriteDbHandler(writeDbHandler4MethodCall);
        writeDbHandler4MethodCall.setSpringControllerMethodHashSet(springControllerMethodHashSet);
        writeDbHandler4MethodCall.setWithAnnotationMethodHashSet(withAnnotationMethodHashSet);
        writeDbHandler4MethodCall.setWithInfoCallIdSet(withInfoCallIdSet);
        writeDbHandler4MethodCall.setWithGenericsTypeMethodHash(withGenericsTypeMethodHash);
        writeDbHandler4MethodCall.setMyBatisMapperSet(myBatisMapperSet);
        writeDbHandler4MethodCall.setMyBatisMapperMethodWriteSet(myBatisMapperMethodWriteSet);
        if (!writeDbHandler4MethodCall.handle(javaCGOutputInfo)) {
            return false;
        }

        // 等待直到任务执行完毕，等待方法调用关系文件写入数据库完毕
        wait4TPEDone();
        return true;
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
                logger.warn("未向数据库写入数据，请检查文件\n{}\n以及写入数据库时需要处理的类名前缀配置 {}", javaCGOutputInfo.getMainFilePath(JavaCGOutPutFileTypeEnum.OPFTE_METHOD_CALL),
                        OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX.getKey());
            }

            if (writeDbHandler.checkFailed()) {
                // 存在读文件写数据库失败
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
}

