package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.conf.ConfManager;
import com.adrninistrator.jacg.dto.annotation.AnnotationInfo4WriteDb;
import com.adrninistrator.jacg.dto.entity.JarInfoEntity;
import com.adrninistrator.jacg.dto.entity.MethodCallEntity;
import com.adrninistrator.jacg.extensions.annotation_attributes.AllAnnotationAttributesFormator;
import com.adrninistrator.jacg.extensions.util.JsonUtil;
import com.adrninistrator.jacg.runner.base.AbstractRunner;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.dto.method.MethodLineNumberInfo;
import com.adrninistrator.javacg.extensions.code_parser.CustomCodeParserInterface;
import com.adrninistrator.javacg.extensions.dto.ExtendedData;
import com.adrninistrator.javacg.stat.JCallGraph;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.File;
import java.util.ArrayList;
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

    // 当类名为以下前缀时，才处理
    private Set<String> allowedClassPrefixSet;

    // 记录自定义处理类
    private List<CustomCodeParserInterface> customCodeParserList;

    // 记录是否读取到文件
    private boolean readFileFlag;

    // 记录是否有写数据库
    private boolean writeDbFlag;

    // 指定java-callgraph2是否需要记录所有的接口调用实现类，及子类调用父类方法，默认不需要
    private boolean javaCGRecordAll = false;

    // Java方法调用关系输出文件路径
    private String callGraphOutputFilePath = null;

    // 注解相关内容输出文件路径
    private String callGraphAnnotationOutputFilePath = null;

    // 方法代码行号输出文件路径
    private String callGraphLineNumberOutputFilePath = null;

    @Override
    public boolean preHandle() {
        if (JACGSqlUtil.isMySQLDb(confInfo.getDbDriverName()) &&
                !confInfo.getDbUrl().contains(JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS)) {
            logger.info("使用MYSQL时，请在{}参数指定{}", ConfigKeyEnum.CKE_DB_URL, JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS);
            return false;
        }

        // 使用多线程，线程数固定为10
        confInfo.setThreadNum(10);

        // 读取其他配置文件
        if (!readOtherConfig()) {
            return false;
        }
        return true;
    }

    @Override
    public void handle() {
        // 执行实际处理
        if (!operate()) {
            // 记录执行失败的任务信息
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

        // 判断是否需要调用java-callgraph2生成jar包的方法调用关系
        if (!callJavaCallGraph()) {
            return false;
        }

        // 读取通过java-callgraph2生成的直接调用关系文件，处理类名与Jar包信息
        if (!handleClassCallAndJarInfo()) {
            return false;
        }

        if (!readFileFlag) {
            if (confInfo.isInputIgnoreOtherPackage()) {
                logger.warn("未从文件读取到内容，请检查文件 {} ，以及配置文件指定的包名 {}", callGraphOutputFilePath, OtherConfigFileUseSetEnum.OCFUSE_IN_ALLOWED_CLASS_PREFIX.getFileName());
            } else {
                logger.warn("未从文件读取到内容，请检查文件 {}", callGraphOutputFilePath);
            }
        }
        if (!writeDbFlag) {
            logger.warn("未向数据库写入数据，请检查文件内容 {}", callGraphOutputFilePath);
        }

        // 查找类名相同但包名不同的类
        if (!dbOperWrapper.findDuplicateClass()) {
            return false;
        }

        // 将类名表中的同名类更新为使用完整类名
        if (!dbOperWrapper.updateAllSimpleName2Full()) {
            return false;
        }

        // 处理类与方法注解信息
        if (!handleAnnotations()) {
            return false;
        }

        // 处理方法代码行号
        if (!handleMethodLineNumber()) {
            return false;
        }

        // 创建线程，参数指定为null，不调小实际创建的线程数
        createThreadPoolExecutor(null);

        // 处理通过java-callgraph2生成的方法调用关系文件
        if (!handleMethodCall()) {
            return false;
        }

        // 等待直到任务执行完毕
        wait4TPEDone();

        if (confInfo.isDbUseH2()) {
            // 显示H2数据库JDBC URL
            printH2JdbcUrl();
        }

        return true;
    }

    // 判断是否需要调用java-callgraph2生成jar包的方法调用关系
    private boolean callJavaCallGraph() {
        logger.info("尝试调用java-callgraph2生成jar包的方法调用关系 {}", confInfo.getCallGraphJarList());

        String[] array = getJarArray();
        for (String jarName : array) {
            if (!new File(jarName).exists()) {
                logger.error("文件或目录不存在 {}", jarName);
                return false;
            }
        }

        if (isJavaCGRecordAll()) {
            // 指定需要记录所有的接口调用实现类，及子类调用父类方法
            JCallGraph.setRecordAll();
        }

        if (confInfo.isInputIgnoreOtherPackage()) {
            String mergeClassInJarPackage = StringUtils.join(allowedClassPrefixSet, JavaCGConstants.FLAG_HASHTAG);
            // 设置合并jar/war包中的class文件时，需要合并的包名
            JCallGraph.setMergeClassInJarPackage(mergeClassInJarPackage);
        }

        // 调用java-callgraph2
        JCallGraph jCallGraph = new JCallGraph();
        // 设置对注解属性进行格式化的类
        jCallGraph.setAnnotationAttributesFormator(new AllAnnotationAttributesFormator());

        // 添加自定义处理类
        if (!addCodeParserExtensions(jCallGraph)) {
            return false;
        }

        boolean success = jCallGraph.run(array);
        if (!success) {
            logger.error("调用java-callgraph2生成jar包的方法调用关系失败");
            return false;
        }

        // 处理自定义数据
        if (!handleExtendedData()) {
            return false;
        }

        // 操作完成之前的处理
        beforeDone();

        // 记录Java方法调用关系输出文件路径
        callGraphOutputFilePath = jCallGraph.getOutputFilePath();
        // 记录注解相关内容输出文件路径
        callGraphAnnotationOutputFilePath = jCallGraph.getAnnotationOutputFilePath();
        // 记录方法代码行号输出文件路径
        callGraphLineNumberOutputFilePath = jCallGraph.getMethodLineNumberOutputFilePath();
        return true;
    }

    // 添加用于对代码进行解析的自定义处理类
    private boolean addCodeParserExtensions(JCallGraph jCallGraph) {
        Set<String> codeParserExtensionClasses = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_EXTENSIONS_CODE_PARSER);
        if (JACGUtil.isCollectionEmpty(codeParserExtensionClasses)) {
            logger.info("未指定用于对代码进行解析的类，跳过 {}", OtherConfigFileUseSetEnum.OCFUSE_EXTENSIONS_CODE_PARSER.getFileName());
            return true;
        }

        customCodeParserList = new ArrayList<>(codeParserExtensionClasses.size());

        try {
            for (String extensionClass : codeParserExtensionClasses) {
                CustomCodeParserInterface customCodeParserInterface = JACGUtil.getClassObject(extensionClass, CustomCodeParserInterface.class);
                if (customCodeParserInterface == null) {
                    return false;
                }

                if (customCodeParserList.contains(customCodeParserInterface)) {
                    continue;
                }

                customCodeParserInterface.init();

                customCodeParserList.add(customCodeParserInterface);
                jCallGraph.addCustomCodeParser(customCodeParserInterface);
            }
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
        return true;
    }

    // 处理自定义数据
    private boolean handleExtendedData() {
        if (JACGUtil.isCollectionEmpty(customCodeParserList)) {
            return true;
        }

        List<Object[]> objectList = new ArrayList<>(JACGConstants.DB_INSERT_BATCH_SIZE);
        for (CustomCodeParserInterface customCodeParserInterface : customCodeParserList) {
            List<ExtendedData> extendedDataList = customCodeParserInterface.getExtendedDataList();
            if (JACGUtil.isCollectionEmpty(extendedDataList)) {
                continue;
            }

            // 插入自定义数据
            logger.info("自定义数据 {}", customCodeParserInterface.getClass().getName());
            if (!insertExtendedData(extendedDataList, objectList)) {
                logger.error("插入自定义数据失败 {}", customCodeParserInterface.getClass().getName());
                return false;
            }
        }
        return true;
    }

    // 操作完成之前的处理
    private void beforeDone() {
        if (JACGUtil.isCollectionEmpty(customCodeParserList)) {
            return;
        }

        for (CustomCodeParserInterface customCodeParserInterface : customCodeParserList) {
            customCodeParserInterface.beforeDone();
        }
    }

    // 插入自定义数据
    private boolean insertExtendedData(List<ExtendedData> extendedDataList, List<Object[]> objectList) {
        String sql = dbOperWrapper.genAndCacheInsertSql(JACGConstants.SQL_KEY_INSERT_EXTENDED_DATA,
                DbInsertMode.DIME_INSERT,
                JACGConstants.TABLE_PREFIX_EXTENDED_DATA,
                JACGConstants.TABLE_COLUMNS_EXTENDED_DATA);

        // 分批插入数据
        int extendedDataListSize = extendedDataList.size();
        int insertTimes = (extendedDataListSize + JACGConstants.DB_INSERT_BATCH_SIZE - 1) / JACGConstants.DB_INSERT_BATCH_SIZE;

        for (int i = 0; i < insertTimes; i++) {
            for (int j = 0; j < JACGConstants.DB_INSERT_BATCH_SIZE; j++) {
                int seq = i * JACGConstants.DB_INSERT_BATCH_SIZE + j;
                if (seq >= extendedDataListSize) {
                    break;
                }
                ExtendedData extendedData = extendedDataList.get(seq);

                Object[] object = new Object[]{extendedData.getCallId(), extendedData.getDataType(), extendedData.getDataValue()};
                objectList.add(object);
            }
            logger.info("写入数据库，自定义数据表 {}", objectList.size());
            boolean success = dbOperator.batchInsert(sql, objectList);
            objectList.clear();

            if (!success) {
                return false;
            }
        }

        return true;
    }

    // 创建数据库表
    private boolean createTables() {
        String classNameSql = readCreateTableSql(JACGConstants.FILE_SQL_CLASS_NAME);
        String classAnnotationSql = readCreateTableSql(JACGConstants.FILE_SQL_CLASS_ANNOTATION);
        String methodAnnotationSql = readCreateTableSql(JACGConstants.FILE_SQL_METHOD_ANNOTATION);
        String methodCallSql = readCreateTableSql(JACGConstants.FILE_SQL_METHOD_CALL);
        String methodLineNumberSql = readCreateTableSql(JACGConstants.FILE_SQL_METHOD_LINE_NUMBER);
        String jarInfoSql = readCreateTableSql(JACGConstants.FILE_SQL_JAR_INFO);
        String extendedDataSql = readCreateTableSql(JACGConstants.FILE_SQL_EXTENDED_DATA);
        String manualAddExtendedDataSql = readCreateTableSql(JACGConstants.FILE_SQL_MANUAL_ADD_EXTENDED_DATA);

        if (StringUtils.isAnyBlank(
                classNameSql,
                classAnnotationSql,
                methodAnnotationSql,
                methodCallSql,
                methodLineNumberSql,
                jarInfoSql,
                extendedDataSql,
                manualAddExtendedDataSql)) {
            logger.error("存在创建数据库表的sql语句为空");
            return false;
        }

        if (!dbOperator.createTable(classNameSql) ||
                !dbOperator.createTable(classAnnotationSql) ||
                !dbOperator.createTable(methodAnnotationSql) ||
                !dbOperator.createTable(methodCallSql) ||
                !dbOperator.createTable(methodLineNumberSql) ||
                !dbOperator.createTable(jarInfoSql) ||
                !dbOperator.createTable(extendedDataSql) ||
                !dbOperator.createTable(manualAddExtendedDataSql)) {
            return false;
        }

        return true;
    }

    private String readCreateTableSql(String sqlFileName) {
        String sqlFilePath = ConfManager.getInputRootPath() + InputDirEnum.IDE_SQL.getDirName() + "/" + sqlFileName;
        String sql = JACGFileUtil.readFile2String(sqlFilePath);
        if (StringUtils.isBlank(sql)) {
            logger.error("文件内容为空 {}", sqlFilePath);
            return null;
        }

        sql = sql.replace(JACGConstants.APPNAME_IN_SQL, confInfo.getAppName());

        if (confInfo.isDbUseH2()) {
            // 使用H2数据库时，对建表的SQL语句进行处理
            sql = sql.replace("ENGINE=InnoDB", "")
                    .replace("COLLATE=utf8_bin", "")
                    .replace(" text ", " varchar(" + JACGConstants.DB_TEXT_MAX_CHARACTER_SIZE + ") ");
        }

        logger.info("建表sql: {}", sql);
        return sql;
    }

    // 清理数据库表
    private boolean truncateTables() {
        if (!dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_CLASS_NAME + confInfo.getAppName()) ||
                !dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_CLASS_ANNOTATION + confInfo.getAppName()) ||
                !dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName()) ||
                !dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION + confInfo.getAppName()) ||
                !dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_METHOD_LINE_NUMBER + confInfo.getAppName()) ||
                !dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_JAR_INFO + confInfo.getAppName()) ||
                !dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_EXTENDED_DATA + confInfo.getAppName())) {
            // TABLE_PREFIX_MANUAL_ADD_EXTENDED_DATA，不清除数据
            return false;
        }
        return true;
    }

    // 读取其他配置文件
    private boolean readOtherConfig() {
        if (confInfo.isInputIgnoreOtherPackage()) {
            allowedClassPrefixSet = configureWrapper.getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_IN_ALLOWED_CLASS_PREFIX);
            if (JACGUtil.isCollectionEmpty(allowedClassPrefixSet)) {
                logger.error("读取文件不存在或内容为空 {}", OtherConfigFileUseSetEnum.OCFUSE_IN_ALLOWED_CLASS_PREFIX.getFileName());
                return false;
            }
        }
        return true;
    }

    // 读取通过java-callgraph2生成的直接调用关系文件，处理类名与Jar包信息
    private boolean handleClassCallAndJarInfo() {
        // 已写入数据库的完整类名
        Map<String, Boolean> fullClassNameMap = new HashMap<>(JACGConstants.DB_INSERT_BATCH_SIZE);

        // 记录完整类名
        List<String> fullClassNameList = new ArrayList<>(JACGConstants.DB_INSERT_BATCH_SIZE);

        // 记录Jar包信息
        Map<Integer, JarInfoEntity> jarInfoMap = new HashMap<>();

        try (BufferedReader br = JACGFileUtil.genBufferedReader(callGraphOutputFilePath)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                if (!readFileFlag) {
                    readFileFlag = true;
                }

                if (line.startsWith(JavaCGConstants.FILE_KEY_CLASS_PREFIX)) {
                    // 处理一个类名
                    if (!handleOneClassCall(line, fullClassNameMap, fullClassNameList)) {
                        return false;
                    }
                } else if (line.startsWith(JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX)) {
                    // 处理一个Jar包信息，类型为jar包
                    handleOneJarInfo(line, true, jarInfoMap);
                } else if (line.startsWith(JavaCGConstants.FILE_KEY_DIR_INFO_PREFIX)) {
                    // 处理一个Jar包信息，类型为文件
                    handleOneJarInfo(line, false, jarInfoMap);
                }
            }

            // 结束前将剩余数据写入数据库
            if (!writeClassName2Db(fullClassNameList)) {
                return false;
            }

            // 将Jar包信息数据写入数据库
            writeJarInfo2Db(jarInfoMap);

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理一个类名
    private boolean handleOneClassCall(String line, Map<String, Boolean> fullClassNameMap, List<String> fullClassNameList) {
        int indexBlank = line.indexOf(JACGConstants.FLAG_SPACE);

        String callerFullClassName = line.substring(JACGConstants.FILE_KEY_PREFIX_LENGTH, indexBlank).trim();
        String calleeFullClassName = line.substring(indexBlank + 1).trim();

        logger.debug("[{}] [{}]", callerFullClassName, calleeFullClassName);

        return handleClassName(callerFullClassName, fullClassNameMap, fullClassNameList) &&
                handleClassName(calleeFullClassName, fullClassNameMap, fullClassNameList);
    }

    private boolean handleClassName(String fullClassName, Map<String, Boolean> fullClassNameMap, List<String> fullClassNameList) {
        // 根据类名前缀判断是否需要处理
        if (confInfo.isInputIgnoreOtherPackage() && !isAllowedClassPrefix(fullClassName)) {
            return true;
        }

        // 通过java-callgraph2生成的直接类引用关系存在重复，进行去重
        if (fullClassNameMap.putIfAbsent(fullClassName, Boolean.TRUE) == null) {
            fullClassNameList.add(fullClassName);

            if (fullClassNameList.size() >= JACGConstants.DB_INSERT_BATCH_SIZE) {
                if (!writeClassName2Db(fullClassNameList)) {
                    return false;
                }
            }
        }

        return true;
    }

    private boolean writeClassName2Db(List<String> fullClassNameList) {
        if (fullClassNameList.isEmpty()) {
            return true;
        }

        logger.info("写入数据库，保存类名信息表 {}", fullClassNameList.size());

        if (!writeDbFlag) {
            writeDbFlag = true;
        }

        String sql = dbOperWrapper.genAndCacheInsertSql(JACGConstants.SQL_KEY_INSERT_CLASS_NAME,
                DbInsertMode.DIME_INSERT,
                JACGConstants.TABLE_PREFIX_CLASS_NAME,
                JACGConstants.TABLE_COLUMNS_CLASS_NAME);

        List<Object[]> objectList = new ArrayList<>(fullClassNameList.size());
        for (String fullClassName : fullClassNameList) {
            String simpleClassName = JACGUtil.getSimpleClassNameFromFull(fullClassName);

            Object[] object = new Object[]{fullClassName, simpleClassName};
            objectList.add(object);
        }

        boolean success = dbOperator.batchInsert(sql, objectList);
        fullClassNameList.clear();
        return success;
    }

    // 处理一个Jar包信息
    private void handleOneJarInfo(String line, boolean isJar, Map<Integer, JarInfoEntity> jarInfoMap) {
        int indexSpace = line.indexOf(JACGConstants.FLAG_SPACE);

        String jarNumStr = line.substring(JACGConstants.FILE_KEY_PREFIX_LENGTH, indexSpace).trim();
        String jarFilePath = line.substring(indexSpace + 1).trim();

        JarInfoEntity jarInfoEntity = new JarInfoEntity(jarFilePath, isJar ? JACGConstants.JAR_TYPE_JAR : JACGConstants.JAR_TYPE_DIR);
        jarInfoMap.put(Integer.valueOf(jarNumStr), jarInfoEntity);
    }

    // 将Jar包信息数据写入数据库
    private boolean writeJarInfo2Db(Map<Integer, JarInfoEntity> jarInfoMap) {
        if (jarInfoMap.isEmpty()) {
            logger.error("Jar包信息为空");
            return false;
        }

        logger.info("写入数据库，保存Jar包信息 {}", jarInfoMap.size());

        String sql = dbOperWrapper.genAndCacheInsertSql(JACGConstants.SQL_KEY_INSERT_JAR_INFO,
                DbInsertMode.DIME_INSERT,
                JACGConstants.TABLE_PREFIX_JAR_INFO,
                JACGConstants.TABLE_COLUMNS_JAR_INFO);

        List<Object[]> objectList = new ArrayList<>(jarInfoMap.size());
        for (Map.Entry<Integer, JarInfoEntity> jarInfoEntry : jarInfoMap.entrySet()) {
            Integer jarNum = jarInfoEntry.getKey();
            JarInfoEntity jarInfoEntity = jarInfoEntry.getValue();
            String jarFilePath = jarInfoEntity.getJarFilePath();

            String lastModified = "";
            String jarFileHash = "";

            if (JACGConstants.JAR_TYPE_JAR.equals(jarInfoEntity.getJarType())) {
                if (!JACGFileUtil.isFileExists(jarFilePath)) {
                    logger.error("Jar包文件不存在: {}", jarFilePath);
                    return false;
                }

                // 为jar包时，获取文件修改时间及HASH
                lastModified = String.valueOf(JACGFileUtil.getFileLastModified(jarFilePath));
                jarFileHash = JACGFileUtil.getFileMd5(jarFilePath);
            }

            Object[] object = new Object[]{jarNum, jarInfoEntity.getJarType(), JACGUtil.genHashWithLen(jarFilePath), jarFilePath, lastModified, jarFileHash};
            objectList.add(object);
        }

        return dbOperator.batchInsert(sql, objectList);
    }

    // 处理方法注解信息
    private boolean handleAnnotations() {
        // 记录方法注解信息列表
        List<AnnotationInfo4WriteDb> methodAnnotationInfoList = new ArrayList<>(JACGConstants.DB_INSERT_BATCH_SIZE);

        // 记录类注解信息列表
        List<AnnotationInfo4WriteDb> classAnnotationInfoList = new ArrayList<>(JACGConstants.DB_INSERT_BATCH_SIZE);

        try (BufferedReader br = JACGFileUtil.genBufferedReader(callGraphAnnotationOutputFilePath)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                if (line.startsWith(JavaCGConstants.FILE_KEY_METHOD_PREFIX)) {
                    // 处理一个方法注解信息
                    if (!handleOneAnnotationInfo(line, true, methodAnnotationInfoList)) {
                        return false;
                    }
                } else if (line.startsWith(JavaCGConstants.FILE_KEY_CLASS_PREFIX)) {
                    // 处理一个类注解信息
                    if (!handleOneAnnotationInfo(line, false, classAnnotationInfoList)) {
                        return false;
                    }
                } else {
                    logger.error("注解文件内容开头非法 {} {}", callGraphAnnotationOutputFilePath, line);
                }
            }

            // 结束前将剩余数据写入数据库
            if (!writeAnnotationInfo2Db(true, methodAnnotationInfoList) ||
                    !writeAnnotationInfo2Db(false, classAnnotationInfoList)) {
                return false;
            }

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理方法代码行号
    private boolean handleMethodLineNumber() {
        // 记录方法行号列表
        List<MethodLineNumberInfo> methodLineNumberList = new ArrayList<>(JACGConstants.DB_INSERT_BATCH_SIZE);

        try (BufferedReader br = JACGFileUtil.genBufferedReader(callGraphLineNumberOutputFilePath)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                // 处理一条方法行号
                if (!handleOneMethodLineNumber(line, methodLineNumberList)) {
                    return false;
                }
            }

            // 结束前将剩余数据写入数据库
            if (!writeMethodLineNumber2Db(methodLineNumberList)) {
                return false;
            }

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    /**
     * 处理一个注解信息
     *
     * @param line
     * @param methodOrClass              true: 处理方法注解信息 false: 处理类注解信息
     * @param annotationInfo4WriteDbList
     * @return
     */
    private boolean handleOneAnnotationInfo(String line, boolean methodOrClass, List<AnnotationInfo4WriteDb> annotationInfo4WriteDbList) {
        // 拆分时限制列数，最后一列注解属性中可能出现空格
        // TODO 后续修改为使用\t
        String[] array = line.split(JACGConstants.FLAG_SPACE, JavaCGConstants.ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE);
        if (array.length < JavaCGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE) {
            logger.error("保存注解信息文件的列数非法 {} [{}]", array.length, line);
            return false;
        }

        String classOrMethodName = array[1];
        String annotationName = array[2];

        // 根据类名或完整方法前缀判断是否需要处理
        if (confInfo.isInputIgnoreOtherPackage() && !isAllowedClassPrefix(classOrMethodName)) {
            return true;
        }

        AnnotationInfo4WriteDb annotationInfo4WriteDb;
        if (array.length > JavaCGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE) {
            // 当前行的注解信息有属性
            String attributeName = array[3];
            String attributeValue = array[4];
            annotationInfo4WriteDb = new AnnotationInfo4WriteDb(classOrMethodName, annotationName, attributeName, attributeValue);
        } else {
            // 当前行的注解信息无属性，将属性名称字段设为空字符串，代表无属性
            annotationInfo4WriteDb = new AnnotationInfo4WriteDb(classOrMethodName, annotationName, "", null);
        }

        // 记录注解信息并尝试写入数据库
        return addAnnotationInfoAndTryWriteDb(annotationInfo4WriteDb, methodOrClass, annotationInfo4WriteDbList);
    }

    /**
     * 记录注解信息并尝试写入数据库
     *
     * @param annotationInfo4WriteDb
     * @param methodOrClass
     * @param annotationInfo4WriteDbList
     * @return
     */
    private boolean addAnnotationInfoAndTryWriteDb(AnnotationInfo4WriteDb annotationInfo4WriteDb, boolean methodOrClass, List<AnnotationInfo4WriteDb> annotationInfo4WriteDbList) {
        // 当发现新的注解信息时，需要先判断注解信息列表是否达到最大数量，若是则写入数据库并清空注解信息列表
        if (annotationInfo4WriteDbList.size() >= JACGConstants.DB_INSERT_BATCH_SIZE) {
            if (!writeAnnotationInfo2Db(methodOrClass, annotationInfo4WriteDbList)) {
                return false;
            }
        }

        // 在注解信息列表之后，再在注解信息列表中记录新的值
        annotationInfo4WriteDbList.add(annotationInfo4WriteDb);
        return true;
    }

    // 处理一条方法行号
    private boolean handleOneMethodLineNumber(String line, List<MethodLineNumberInfo> methodLineNumberList) {
        String[] array = line.split(JACGConstants.FLAG_SPACE);
        if (array.length != JavaCGConstants.LINE_NUMBER_COLUMN_NUM) {
            logger.error("保存方法行号信息文件的列数非法 {} [{}]", array.length, line);
            return false;
        }

        String fullMethod = array[0];

        // 根据完整方法前缀判断是否需要处理
        if (confInfo.isInputIgnoreOtherPackage() && !isAllowedClassPrefix(fullMethod)) {
            return true;
        }

        MethodLineNumberInfo methodLineNumberInfo = new MethodLineNumberInfo();
        methodLineNumberInfo.setFullMethod(fullMethod);
        methodLineNumberInfo.setMinLineNumber(Integer.parseInt(array[1]));
        methodLineNumberInfo.setMaxLineNumber(Integer.parseInt(array[2]));
        methodLineNumberList.add(methodLineNumberInfo);

        if (methodLineNumberList.size() >= JACGConstants.DB_INSERT_BATCH_SIZE) {
            if (!writeMethodLineNumber2Db(methodLineNumberList)) {
                return false;
            }
        }

        return true;
    }

    private boolean writeAnnotationInfo2Db(boolean methodOrClass, List<AnnotationInfo4WriteDb> annotationInfo4WriteDbList) {
        if (annotationInfo4WriteDbList.isEmpty()) {
            return true;
        }

        logger.info("{}注解信息写入数据库 {}", (methodOrClass ? "方法" : "类"), annotationInfo4WriteDbList.size());

        String sql;
        List<Object[]> objectList;

        if (methodOrClass) {
            // 写入方法注解信息
            sql = dbOperWrapper.genAndCacheInsertSql(JACGConstants.SQL_KEY_INSERT_METHOD_ANNOTATION,
                    DbInsertMode.DIME_INSERT,
                    JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION,
                    JACGConstants.TABLE_COLUMNS_METHOD_ANNOTATION);

            objectList = new ArrayList<>(annotationInfo4WriteDbList.size());
            for (AnnotationInfo4WriteDb annotationInfo4WriteDb : annotationInfo4WriteDbList) {
                String fullMethod = annotationInfo4WriteDb.getClassOrMethodName();
                String annotationName = annotationInfo4WriteDb.getAnnotationName();
                String methodHash = JACGUtil.genHashWithLen(fullMethod);

                Object[] object = new Object[]{
                        methodHash,
                        annotationName,
                        annotationInfo4WriteDb.getAttributeName(),
                        annotationInfo4WriteDb.getAttributeValue(),
                        fullMethod
                };
                objectList.add(object);
            }
        } else {
            // 写入类注解信息
            sql = dbOperWrapper.genAndCacheInsertSql(JACGConstants.SQL_KEY_INSERT_CLASS_ANNOTATION,
                    DbInsertMode.DIME_INSERT,
                    JACGConstants.TABLE_PREFIX_CLASS_ANNOTATION,
                    JACGConstants.TABLE_COLUMNS_CLASS_ANNOTATION);

            objectList = new ArrayList<>(annotationInfo4WriteDbList.size());
            for (AnnotationInfo4WriteDb annotationInfo4WriteDb : annotationInfo4WriteDbList) {
                Object[] object = new Object[]{
                        annotationInfo4WriteDb.getClassOrMethodName(),
                        annotationInfo4WriteDb.getAnnotationName(),
                        annotationInfo4WriteDb.getAttributeName(),
                        annotationInfo4WriteDb.getAttributeValue()
                };
                objectList.add(object);
            }
        }

        boolean success = dbOperator.batchInsert(sql, objectList);
        if (!success) {
            if (JACGConstants.DB_INSERT_BATCH_SIZE == 1) {
                logger.error("插入注解信息失败 {}", JsonUtil.getJsonStr(annotationInfo4WriteDbList.get(0)));
            } else {
                logger.error("插入注解信息失败，为了定位重复的注解信息，可在JVM参数中指定 -D{}=1", JACGConstants.PROPERTY_DB_INSERT_BATCH_SIZE);
            }
        }

        annotationInfo4WriteDbList.clear();
        return success;
    }

    private boolean writeMethodLineNumber2Db(List<MethodLineNumberInfo> methodLineNumberList) {
        if (methodLineNumberList.isEmpty()) {
            return true;
        }

        logger.info("写入数据库，方法代码行号信息表 {}", methodLineNumberList.size());

        List<Object[]> objectList;

        String sql = dbOperWrapper.genAndCacheInsertSql(JACGConstants.SQL_KEY_INSERT_METHOD_LINE_NUMBER,
                DbInsertMode.DIME_INSERT,
                JACGConstants.TABLE_PREFIX_METHOD_LINE_NUMBER,
                JACGConstants.TABLE_COLUMNS_METHOD_LINE_NUMBER);

        objectList = new ArrayList<>(methodLineNumberList.size());
        for (MethodLineNumberInfo methodLineNumberInfo : methodLineNumberList) {
            int minLineNumber = methodLineNumberInfo.getMinLineNumber();
            int maxLineNumber = methodLineNumberInfo.getMaxLineNumber();
            String fullMethod = methodLineNumberInfo.getFullMethod();

            String fullClassName = JACGUtil.getFullClassNameFromMethod(fullMethod);
            String simpleClassName = dbOperWrapper.getFullOrSimpleClassName(fullClassName);
            String methodHash = JACGUtil.genHashWithLen(fullMethod);
            Object[] object = new Object[]{
                    methodHash,
                    simpleClassName,
                    minLineNumber,
                    maxLineNumber,
                    fullMethod
            };
            objectList.add(object);
        }

        boolean success = dbOperator.batchInsert(sql, objectList);
        methodLineNumberList.clear();
        return success;
    }

    // 处理通过java-callgraph2生成的方法调用关系文件
    private boolean handleMethodCall() {
        // 记录方法调用列表
        List<MethodCallEntity> methodCallList = new ArrayList<>(JACGConstants.DB_INSERT_BATCH_SIZE);

        try (BufferedReader br = JACGFileUtil.genBufferedReader(callGraphOutputFilePath)) {
            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                if (line.startsWith(JavaCGConstants.FILE_KEY_METHOD_PREFIX)) {
                    // 处理一条方法调用
                    if (!handleOneMethodCall(line, methodCallList)) {
                        return false;
                    }
                }
            }

            // 结束前将剩余数据写入数据库
            writeMethodCall2Db(methodCallList);

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理一条方法调用
    private boolean handleOneMethodCall(String line, List<MethodCallEntity> methodCallList) {
        String[] methodCallArray = line.split(JACGConstants.FLAG_SPACE);
        if (methodCallArray.length != 5) {
            logger.error("方法调用信息非法 [{}] [{}]", line, methodCallArray.length);
            return false;
        }

//    todo    这里要确认怎么处理
        String callIdStr = methodCallArray[0].substring(JACGConstants.FILE_KEY_PREFIX_LENGTH);
        String callerFullMethod = methodCallArray[1];
        String calleeFullMethod = methodCallArray[2];
        String strCallerLineNum = methodCallArray[3];
        String callerJarNum = methodCallArray[4];

        if (!JACGUtil.isNumStr(callIdStr)) {
            logger.error("方法调用ID非法 [{}] [{}]", line, callIdStr);
            return false;
        }

        if (!JACGUtil.isNumStr(strCallerLineNum)) {
            logger.error("方法调用信息行号非法 [{}] [{}]", line, strCallerLineNum);
            return false;
        }

        if (!JACGUtil.isNumStr(callerJarNum)) {
            logger.error("Jar包序号非法 [{}] [{}]", line, callerJarNum);
            return false;
        }

        int callerLineNum = Integer.parseInt(strCallerLineNum);
//   todo     换成javacg中的常量
        int indexCalleeLeftBracket = calleeFullMethod.indexOf(JACGConstants.FLAG_LEFT_BRACKET);
//   todo     换成javacg中的常量
        int indexCalleeRightBracket = calleeFullMethod.indexOf(JACGConstants.FLAG_RIGHT_BRACKET);

//   todo     +1换成换成javacg中的常量.lenth()
        String callType = calleeFullMethod.substring(indexCalleeLeftBracket + 1, indexCalleeRightBracket);

//   todo     +1换成换成javacg中的常量.lenth()
        String finalCalleeFullMethod = calleeFullMethod.substring(indexCalleeRightBracket + 1).trim();
        int callId = Integer.parseInt(callIdStr);

        // 生成方法调用数据
        MethodCallEntity methodCallEntity = dbOperWrapper.genMethodCallEntity(callType, callerFullMethod, finalCalleeFullMethod, callId, callerLineNum, callerJarNum);

        if (StringUtils.equals(methodCallEntity.getCallerMethodHash(), methodCallEntity.getCalleeMethodHash())) {
            // 对于递归调用，不写入数据库，防止查询时出现死循环
            logger.info("递归调用不写入数据库 {}", line);
            return true;
        }

        // 根据类名前缀判断是否需要处理
        if (confInfo.isInputIgnoreOtherPackage() &&
                (!isAllowedClassPrefix(methodCallEntity.getCallerFullMethod()) || !isAllowedClassPrefix(methodCallEntity.getCalleeFullMethod()))) {
            return true;
        }

        methodCallList.add(methodCallEntity);

        if (methodCallList.size() >= JACGConstants.DB_INSERT_BATCH_SIZE) {
            writeMethodCall2Db(methodCallList);
        }

        return true;
    }

    private void writeMethodCall2Db(List<MethodCallEntity> methodCallList) {
        if (methodCallList.isEmpty()) {
            return;
        }

        List<Object[]> tmpMethodCallList = dbOperWrapper.genMethodCallList(methodCallList);
        methodCallList.clear();

        // 等待直到允许任务执行
        wait4TPEExecute();

        threadPoolExecutor.execute(() -> {
            logger.info("写入数据库，方法调用关系表 {}", tmpMethodCallList.size());

            if (!dbOperWrapper.writeMethodCall2Db(tmpMethodCallList)) {
                // 记录执行失败的任务信息
                recordTaskFail();
            }
        });
    }

    /**
     * 判断当类名为以下前缀时，才处理
     *
     * @param className 类名，或完整方法（类名+方法名+参数）
     * @return true: 需要处理，false: 忽略
     */
    private boolean isAllowedClassPrefix(String className) {
        for (String allowedClassPrefix : allowedClassPrefixSet) {
            if (className.startsWith(allowedClassPrefix)) {
                return true;
            }
        }
        return false;
    }

    // 显示H2数据库JDBC URL
    private void printH2JdbcUrl() {
        String h2DbFilePath = JACGFileUtil.getCanonicalPath(getH2DbFile());
        if (h2DbFilePath == null) {
            return;
        }
        String h2DbFilePathWithoutExt = JACGUtil.getFileNameWithOutExt(h2DbFilePath, JACGConstants.H2_FILE_EXT);
        logger.info("可用于连接H2数据库的JDBC URL:\n{}{}\n{}", JACGConstants.H2_PROTOCOL, h2DbFilePathWithoutExt, h2DbFilePath);
    }

    public boolean isJavaCGRecordAll() {
        return javaCGRecordAll;
    }

    public void setJavaCGRecordAll(boolean javaCGRecordAll) {
        this.javaCGRecordAll = javaCGRecordAll;
    }
}

