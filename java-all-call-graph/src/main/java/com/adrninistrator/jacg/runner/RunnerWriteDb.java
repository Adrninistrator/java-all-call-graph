package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dto.annotation.AnnotationInfo4Write;
import com.adrninistrator.jacg.dto.JarInfo;
import com.adrninistrator.jacg.dto.MethodCallEntity;
import com.adrninistrator.jacg.extensions.util.JsonUtil;
import com.adrninistrator.jacg.runner.base.AbstractRunner;
import com.adrninistrator.jacg.util.FileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.jacg.util.SqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.extensions.code_parser.CustomCodeParserInterface;
import com.adrninistrator.javacg.extensions.dto.ExtendedData;
import com.adrninistrator.javacg.stat.JCallGraph;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description: 将通过java-callgraph2生成的直接调用关系文件写入数据库
 */

public class RunnerWriteDb extends AbstractRunner {

    private static final Logger logger = LoggerFactory.getLogger(RunnerWriteDb.class);

    // 当类名为以下前缀时，才处理
    private Set<String> allowedClassPrefixSet;

    // 已写入数据库的完整类名
    private Map<String, Boolean> fullClassNameMap = new HashMap<>(JACGConstants.BATCH_SIZE);

    // 记录完整类名
    private List<String> fullClassNameList = new ArrayList<>(JACGConstants.BATCH_SIZE);

    // 类名相同但包名不同的类名
    private Set<String> duplicateClassNameSet = new HashSet<>();

    // 记录方法注解信息
    private List<AnnotationInfo4Write> methodAnnotationInfoList = new ArrayList<>(JACGConstants.BATCH_SIZE);

    // 记录类注解信息
    private List<AnnotationInfo4Write> classAnnotationInfoList = new ArrayList<>(JACGConstants.BATCH_SIZE);

    // 记录方法调用
    private List<MethodCallEntity> methodCallList = new ArrayList<>(JACGConstants.BATCH_SIZE);

    // 记录Jar包信息
    private Map<Integer, JarInfo> jarInfoMap = new HashMap<>();

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

    static {
        runner = new RunnerWriteDb();
    }

    // 预检查
    @Override
    public boolean preCheck() {
        if (confInfo.isDbUseH2() && !checkH2DbFile()) {
            // 使用H2数据库时，检查H2数据库文件
            return false;
        }

        return true;
    }

    @Override
    public boolean init() {
        if (SqlUtil.isMySQLDb(confInfo.getDbDriverName()) &&
                !confInfo.getDbUrl().contains(JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS)) {
            logger.info("使用MYSQL时，请在{}参数指定{}", JACGConstants.KEY_DB_URL, JACGConstants.MYSQL_REWRITEBATCHEDSTATEMENTS);
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
    public void operate() {
        if (!doOperate()) {
            someTaskFail = true;
        }
    }

    // 检查H2数据库文件
    private boolean checkH2DbFile() {
        File h2DbFile = getH2DbFile();
        if (!h2DbFile.exists()) {
            return true;
        }

        // 数据库文件存在
        if (!h2DbFile.isFile()) {
            logger.error("H2数据库文件不是文件 {}", FileUtil.getCanonicalPath(h2DbFile));
            return false;
        }

        // 检查H2数据库文件是否可写
        return checkH2DbFileWritable(h2DbFile);
    }

    private boolean doOperate() {
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
                logger.warn("未从文件读取到内容，请检查文件 {} ，以及配置文件指定的包名 {}", callGraphOutputFilePath, JACGConstants.FILE_IN_ALLOWED_CLASS_PREFIX);
            } else {
                logger.warn("未从文件读取到内容，请检查文件 {}", callGraphOutputFilePath);
            }
        }
        if (!writeDbFlag) {
            logger.warn("未向数据库写入数据，请检查文件内容 {}", callGraphOutputFilePath);
        }

        // 查找类名相同但包名不同的类
        if (!findDuplicateClass()) {
            return false;
        }

        // 读取方法注解信息
        if (!handleMethodAnnotation()) {
            return false;
        }

        // 创建线程
        createThreadPoolExecutor();

        // 读取通过java-callgraph2生成的直接调用关系文件，处理方法调用
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

        // 记录Java方法调用关系输出文件路径
        callGraphOutputFilePath = jCallGraph.getOutputFilePath();
        // 记录注解相关内容输出文件路径
        callGraphAnnotationOutputFilePath = jCallGraph.getAnnotationOutputFilePath();
        return true;
    }

    // 添加用于对代码进行解析的自定义处理类
    private boolean addCodeParserExtensions(JCallGraph jCallGraph) {
        String codeParserExtensionFilePath = JACGConstants.DIR_EXTENSIONS + File.separator + JACGConstants.FILE_EXTENSIONS_CODE_PARSER;

        Set<String> codeParserExtensionClasses = FileUtil.readFile2Set(codeParserExtensionFilePath);
        if (JACGUtil.isCollectionEmpty(codeParserExtensionClasses)) {
            logger.info("未指定用于对代码进行解析的类，跳过 {}", codeParserExtensionFilePath);
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

        List<Object[]> objectList = new ArrayList<>(JACGConstants.BATCH_SIZE);
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

    // 插入自定义数据
    private boolean insertExtendedData(List<ExtendedData> extendedDataList, List<Object[]> objectList) {
        String sqlKey = JACGConstants.SQL_KEY_INSERT_EXTENDED_DATA;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            sql = genAndCacheInsertSql(sqlKey,
                    false,
                    JACGConstants.TABLE_PREFIX_EXTENDED_DATA,
                    JACGConstants.TABLE_COLUMNS_EXTENDED_DATA);
        }

        // 分批插入数据
        int extendedDataListSize = extendedDataList.size();
        int insertTimes = (extendedDataListSize + JACGConstants.BATCH_SIZE - 1) / JACGConstants.BATCH_SIZE;

        for (int i = 0; i < insertTimes; i++) {
            for (int j = 0; j < JACGConstants.BATCH_SIZE; j++) {
                int seq = i * JACGConstants.BATCH_SIZE + j;
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
        String classNameSql = readCreateTableSql(JACGConstants.DIR_SQL + File.separator + JACGConstants.FILE_SQL_CLASS_NAME);
        String methodAnnotationSql = readCreateTableSql(JACGConstants.DIR_SQL + File.separator + JACGConstants.FILE_SQL_METHOD_ANNOTATION);
        String classAnnotationSql = readCreateTableSql(JACGConstants.DIR_SQL + File.separator + JACGConstants.FILE_SQL_CLASS_ANNOTATION);
        String methodCallSql = readCreateTableSql(JACGConstants.DIR_SQL + File.separator + JACGConstants.FILE_SQL_METHOD_CALL);
        String jarInfoSql = readCreateTableSql(JACGConstants.DIR_SQL + File.separator + JACGConstants.FILE_SQL_JAR_INFO);
        String extendedDataSql = readCreateTableSql(JACGConstants.DIR_SQL + File.separator + JACGConstants.FILE_SQL_EXTENDED_DATA);
        String manualAddExtendedDataSql = readCreateTableSql(JACGConstants.DIR_SQL + File.separator + JACGConstants.FILE_SQL_MANUAL_ADD_EXTENDED_DATA);

        if (StringUtils.isAnyBlank(classNameSql, methodAnnotationSql, classAnnotationSql, methodCallSql, jarInfoSql, extendedDataSql, manualAddExtendedDataSql)) {
            return false;
        }

        if (!dbOperator.createTable(classNameSql) ||
                !dbOperator.createTable(methodAnnotationSql) ||
                !dbOperator.createTable(classAnnotationSql) ||
                !dbOperator.createTable(methodCallSql) ||
                !dbOperator.createTable(jarInfoSql) ||
                !dbOperator.createTable(extendedDataSql) ||
                !dbOperator.createTable(manualAddExtendedDataSql)) {
            return false;
        }

        return true;
    }

    private String readCreateTableSql(String sqlFilePath) {
        String sql = FileUtil.readFile2String(sqlFilePath);
        if (StringUtils.isBlank(sql)) {
            logger.error("文件内容为空 {}", sqlFilePath);
            return null;
        }

        sql = sql.replace(JACGConstants.APPNAME_IN_SQL, confInfo.getAppName());

        if (confInfo.isDbUseH2()) {
            // 使用H2数据库时，对建表的SQL语句进行处理
            sql = sql.replace("ENGINE=InnoDB", "")
                    .replace("COLLATE=utf8_bin", "")
                    .replace(" text ", " varchar(3000) ");
        }

        logger.info("建表sql: {}", sql);
        return sql;
    }

    // 清理数据库表
    private boolean truncateTables() {
        if (!dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_CLASS_NAME + confInfo.getAppName()) ||
                !dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName()) ||
                !dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION + confInfo.getAppName()) ||
                !dbOperator.truncateTable(JACGConstants.TABLE_PREFIX_CLASS_ANNOTATION + confInfo.getAppName()) ||
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
            String allowedClassPrefixFile = JACGConstants.DIR_CONFIG + File.separator + JACGConstants.FILE_IN_ALLOWED_CLASS_PREFIX;
            allowedClassPrefixSet = FileUtil.readFile2Set(allowedClassPrefixFile);
            if (JACGUtil.isCollectionEmpty(allowedClassPrefixSet)) {
                logger.error("读取文件不存在或内容为空 {}", allowedClassPrefixFile);
                return false;
            }
        }
        return true;
    }

    // 读取通过java-callgraph2生成的直接调用关系文件，处理类名与Jar包信息
    private boolean handleClassCallAndJarInfo() {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(callGraphOutputFilePath), StandardCharsets.UTF_8))) {
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
                    if (!handleOneClassCall(line)) {
                        return false;
                    }
                } else if (line.startsWith(JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX)) {
                    // 处理一个Jar包信息，类型为jar包
                    handleOneJarInfo(line, true);
                } else if (line.startsWith(JavaCGConstants.FILE_KEY_DIR_INFO_PREFIX)) {
                    // 处理一个Jar包信息，类型为文件
                    handleOneJarInfo(line, false);
                }
            }

            // 结束前将剩余数据写入数据库
            if (!writeClassName2Db()) {
                return false;
            }

            // 将Jar包信息数据写入数据库
            writeJarInfo2Db();

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 查找类名相同但包名不同的类
    private boolean findDuplicateClass() {
        String sqlKey = JACGConstants.SQL_KEY_CN_QUERY_DUPLICATE_CLASS;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            sql = "select " + DC.CN_SIMPLE_NAME + " from " + JACGConstants.TABLE_PREFIX_CLASS_NAME + confInfo.getAppName() +
                    " group by " + DC.CN_SIMPLE_NAME + " having count(" + DC.CN_SIMPLE_NAME + ") > 1";
            cacheSql(sqlKey, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, null);
        if (list == null) {
            return false;
        }

        if (!list.isEmpty()) {
            for (Object object : list) {
                String duplicateClassName = (String) object;
                duplicateClassNameSet.add(duplicateClassName);

                // 将简单类名重复的类名，更新为完整类名
                updateSimpleName2Full(duplicateClassName);
            }
        }
        return true;
    }

    // 将class_name_表的simple_name更新为full_name
    private boolean updateSimpleName2Full(String simpleName) {
        String sqlKey = JACGConstants.SQL_KEY_CN_UPDATE_SIMPLE_2_FULL;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            sql = "update " + JACGConstants.TABLE_PREFIX_CLASS_NAME + confInfo.getAppName() +
                    " set " + DC.CN_SIMPLE_NAME + " = " + DC.CN_FULL_NAME + " where " + DC.CN_SIMPLE_NAME + " = ?";
            cacheSql(sqlKey, sql);
        }

        Integer row = dbOperator.update(sql, new Object[]{simpleName});
        return row != null;
    }

    // 处理一个类名
    private boolean handleOneClassCall(String data) {
        int indexBlank = data.indexOf(JACGConstants.FLAG_SPACE);

        String callerFullClassName = data.substring(JACGConstants.FILE_KEY_PREFIX_LENGTH, indexBlank).trim();
        String calleeFullClassName = data.substring(indexBlank + 1).trim();

        logger.debug("[{}] [{}]", callerFullClassName, calleeFullClassName);

        return handleClassName(callerFullClassName) && handleClassName(calleeFullClassName);
    }

    private boolean handleClassName(String fullClassName) {
        // 根据类名前缀判断是否需要处理
        if (confInfo.isInputIgnoreOtherPackage() && !isAllowedClassPrefix(fullClassName)) {
            return true;
        }

        // 通过java-callgraph2生成的直接类引用关系存在重复，进行去重
        if (fullClassNameMap.putIfAbsent(fullClassName, Boolean.TRUE) == null) {
            fullClassNameList.add(fullClassName);

            if (fullClassNameList.size() >= JACGConstants.BATCH_SIZE) {
                if (!writeClassName2Db()) {
                    return false;
                }
            }
        }

        return true;
    }

    private boolean writeClassName2Db() {
        if (fullClassNameList.isEmpty()) {
            return true;
        }

        logger.info("写入数据库，保存类名信息表 {}", fullClassNameList.size());

        if (!writeDbFlag) {
            writeDbFlag = true;
        }

        String sqlKey = JACGConstants.SQL_KEY_INSERT_CLASS_NAME;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            sql = genAndCacheInsertSql(sqlKey,
                    false,
                    JACGConstants.TABLE_PREFIX_CLASS_NAME,
                    JACGConstants.TABLE_COLUMNS_CLASS_NAME);
        }

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
    private void handleOneJarInfo(String line, boolean isJar) {
        int indexSpace = line.indexOf(JACGConstants.FLAG_SPACE);

        String jarNumStr = line.substring(JACGConstants.FILE_KEY_PREFIX_LENGTH, indexSpace).trim();
        String jarFilePath = line.substring(indexSpace + 1).trim();

        JarInfo jarInfo = new JarInfo();
        jarInfo.setJarFilePath(jarFilePath);
        jarInfo.setJarType(isJar ? JACGConstants.JAR_TYPE_JAR : JACGConstants.JAR_TYPE_DIR);

        jarInfoMap.put(Integer.valueOf(jarNumStr), jarInfo);
    }

    // 将Jar包信息数据写入数据库
    private boolean writeJarInfo2Db() {
        if (jarInfoMap.isEmpty()) {
            logger.error("Jar包信息为空");
            return false;
        }

        logger.info("写入数据库，保存Jar包信息 {}", jarInfoMap.size());

        if (!writeDbFlag) {
            writeDbFlag = true;
        }

        String sqlKey = JACGConstants.SQL_KEY_INSERT_JAR_INFO;
        String sql = sqlCacheMap.get(sqlKey);
        if (sql == null) {
            sql = genAndCacheInsertSql(sqlKey,
                    false,
                    JACGConstants.TABLE_PREFIX_JAR_INFO,
                    JACGConstants.TABLE_COLUMNS_JAR_INFO);
        }

        List<Object[]> objectList = new ArrayList<>(jarInfoMap.size());
        for (Map.Entry<Integer, JarInfo> jarInfoEntry : jarInfoMap.entrySet()) {
            Integer jarNum = jarInfoEntry.getKey();
            JarInfo jarInfo = jarInfoEntry.getValue();
            String jarFilePath = jarInfo.getJarFilePath();

            String lastModified = "";
            String jarFileHash = "";

            if (JACGConstants.JAR_TYPE_JAR.equals(jarInfo.getJarType())) {
                if (!FileUtil.isFileExists(jarFilePath)) {
                    logger.error("Jar包文件不存在: {}", jarFilePath);
                    return false;
                }

                // 为jar包时，获取文件修改时间及HASH
                lastModified = String.valueOf(FileUtil.getFileLastModified(jarFilePath));
                jarFileHash = FileUtil.getFileMd5(jarFilePath);
            }

            Object[] object = new Object[]{jarNum, jarInfo.getJarType(), JACGUtil.genHashWithLen(jarFilePath), jarFilePath, lastModified, jarFileHash};
            objectList.add(object);
        }

        return dbOperator.batchInsert(sql, objectList);
    }

    // 读取方法注解信息
    private boolean handleMethodAnnotation() {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(callGraphAnnotationOutputFilePath), StandardCharsets.UTF_8))) {
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

    /**
     * 处理一个注解信息
     *
     * @param data
     * @param methodOrClass            true: 处理方法注解信息 false: 处理类注解信息
     * @param annotationInfo4WriteList
     * @return
     */
    private boolean handleOneAnnotationInfo(String data, boolean methodOrClass, List<AnnotationInfo4Write> annotationInfo4WriteList) {
        String[] array = data.split(JACGConstants.FLAG_SPACE);
        if (array.length != JavaCGConstants.ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE && array.length != JavaCGConstants.ANNOTATION_COLUMN_NUM_WITHOUT_ATTRIBUTE) {
            logger.error("保存注解信息文件的列数非法 {} [{}]", array.length, data);
            return false;
        }

        String classOrMethodName = array[1];

        // 根据类名或完整方法前缀判断是否需要处理
        if (confInfo.isInputIgnoreOtherPackage() && !isAllowedClassPrefix(classOrMethodName)) {
            return true;
        }

        String annotationName = array[2];
        AnnotationInfo4Write annotationInfo4Write = new AnnotationInfo4Write();
        annotationInfo4Write.setClassOrMethodName(classOrMethodName);
        annotationInfo4Write.setAnnotationName(annotationName);
        annotationInfo4Write.setAnnotationAttributeMap(new HashMap<>());

        if (array.length == JavaCGConstants.ANNOTATION_COLUMN_NUM_WITH_ATTRIBUTE) {
            // 当前行的注解信息有属性
            String annotationAttributeKey = array[3];
            String annotationAttributeValue = JavaCGUtil.decodeAnnotationValue(array[4]);

            // 判断列表中上一条记录与当前记录是否是同一个类的同一个注解信息
            AnnotationInfo4Write getLastSameAnnotation = getLastSameAnnotation(annotationInfo4WriteList, classOrMethodName, annotationName);
            if (getLastSameAnnotation != null) {
                // 列表中上一条记录与当前记录是同一个类的同一个注解信息
                // 增加注解属性信息，不向列表中增加元素
                getLastSameAnnotation.getAnnotationAttributeMap().put(annotationAttributeKey, annotationAttributeValue);
            } else {
                // 列表中上一条记录与当前记录不是同一个类的同一个注解信息
                // 记录注解属性信息
                annotationInfo4Write.getAnnotationAttributeMap().put(annotationAttributeKey, annotationAttributeValue);

                // 向列表中增加元素
                annotationInfo4WriteList.add(annotationInfo4Write);
            }
        } else {
            // 当前行的注解信息没有属性
            // 向列表中增加元素
            annotationInfo4WriteList.add(annotationInfo4Write);
        }

        if (annotationInfo4WriteList.size() >= JACGConstants.BATCH_SIZE) {
            if (!writeAnnotationInfo2Db(methodOrClass, annotationInfo4WriteList)) {
                return false;
            }
        }

        return true;
    }

    /**
     * 获取列表中上一条记录，仅当上一条记录与当前记录是同一个类的同一个注解信息时才返回
     *
     * @param annotationInfo4WriteList
     * @param classOrMethodName
     * @param annotationName
     * @return 非null: 当上一条记录 null: 上一条记录与当前记录不是同一个类的同一个注解信息
     */
    private AnnotationInfo4Write getLastSameAnnotation(List<AnnotationInfo4Write> annotationInfo4WriteList, String classOrMethodName, String annotationName) {
        if (annotationInfo4WriteList.isEmpty()) {
            return null;
        }

        AnnotationInfo4Write lastAnnotationInfo4Write = annotationInfo4WriteList.get(annotationInfo4WriteList.size() - 1);
        if (StringUtils.equals(lastAnnotationInfo4Write.getClassOrMethodName(), classOrMethodName) &&
                StringUtils.equals(lastAnnotationInfo4Write.getAnnotationName(), annotationName)) {
            return lastAnnotationInfo4Write;
        }

        return null;
    }

    private boolean writeAnnotationInfo2Db(boolean methodOrClass, List<AnnotationInfo4Write> annotationInfo4WriteList) {
        if (annotationInfo4WriteList.isEmpty()) {
            return true;
        }

        logger.info("写入数据库，保存方法注解信息表 {}", annotationInfo4WriteList.size());

        if (!writeDbFlag) {
            writeDbFlag = true;
        }

        String sql;
        List<Object[]> objectList;

        if (methodOrClass) {
            // 写入方法注解信息
            String sqlKey = JACGConstants.SQL_KEY_INSERT_METHOD_ANNOTATION;
            sql = sqlCacheMap.get(sqlKey);
            if (sql == null) {
                sql = genAndCacheInsertSql(sqlKey,
                        false,
                        JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION,
                        JACGConstants.TABLE_COLUMNS_METHOD_ANNOTATION);
            }

            objectList = new ArrayList<>(annotationInfo4WriteList.size());
            for (AnnotationInfo4Write annotationInfo4Write : annotationInfo4WriteList) {
                String fullMethod = annotationInfo4Write.getClassOrMethodName();
                String annotationName = annotationInfo4Write.getAnnotationName();
                String methodHash = JACGUtil.genHashWithLen(fullMethod);
                String annotationAttributes = JsonUtil.getJsonStr(annotationInfo4Write.getAnnotationAttributeMap());

                Object[] object = new Object[]{methodHash, annotationName, annotationAttributes, fullMethod};
                objectList.add(object);
            }
        } else {
            // 写入类注解信息
            String sqlKey = JACGConstants.SQL_KEY_INSERT_CLASS_ANNOTATION;
            sql = sqlCacheMap.get(sqlKey);
            if (sql == null) {
                sql = genAndCacheInsertSql(sqlKey,
                        false,
                        JACGConstants.TABLE_PREFIX_CLASS_ANNOTATION,
                        JACGConstants.TABLE_COLUMNS_CLASS_ANNOTATION);
            }

            objectList = new ArrayList<>(annotationInfo4WriteList.size());
            for (AnnotationInfo4Write annotationInfo4Write : annotationInfo4WriteList) {
                String className = annotationInfo4Write.getClassOrMethodName();
                String annotationName = annotationInfo4Write.getAnnotationName();
                String annotationAttributes = JsonUtil.getJsonStr(annotationInfo4Write.getAnnotationAttributeMap());

                Object[] object = new Object[]{className, annotationName, annotationAttributes};
                objectList.add(object);
            }
        }

        boolean success = dbOperator.batchInsert(sql, objectList);
        annotationInfo4WriteList.clear();
        return success;
    }


    // 读取通过java-callgraph2生成的直接调用关系文件，处理方法调用
    private boolean handleMethodCall() {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(callGraphOutputFilePath), StandardCharsets.UTF_8))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                if (line.startsWith(JavaCGConstants.FILE_KEY_METHOD_PREFIX)) {
                    // 处理一条方法调用
                    if (!handleOneMethodCall(line)) {
                        return false;
                    }
                }
            }

            // 结束前将剩余数据写入数据库
            writeMethodCall2Db();

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理一条方法调用
    private boolean handleOneMethodCall(String data) {
        String[] methodCallArray = data.split(JACGConstants.FLAG_SPACE);
        if (methodCallArray.length != 5) {
            logger.error("方法调用信息非法 [{}] [{}]", data, methodCallArray.length);
            return false;
        }

        String callIdStr = methodCallArray[0].substring(JACGConstants.FILE_KEY_PREFIX_LENGTH);
        String callerFullMethod = methodCallArray[1];
        String calleeFullMethod = methodCallArray[2];
        String strCallerLineNum = methodCallArray[3];
        String callerJarNum = methodCallArray[4];

        if (!JACGUtil.isNumStr(callIdStr)) {
            logger.error("方法调用ID非法 [{}] [{}]", data, callIdStr);
            return false;
        }

        if (!JACGUtil.isNumStr(strCallerLineNum)) {
            logger.error("方法调用信息行号非法 [{}] [{}]", data, strCallerLineNum);
            return false;
        }

        if (!JACGUtil.isNumStr(callerJarNum)) {
            logger.error("Jar包序号非法 [{}] [{}]", data, callerJarNum);
            return false;
        }

        int callerLineNum = Integer.parseInt(strCallerLineNum);
        int indexCalleeLeftBracket = calleeFullMethod.indexOf(JACGConstants.FLAG_LEFT_BRACKET);
        int indexCalleeRightBracket = calleeFullMethod.indexOf(JACGConstants.FLAG_RIGHT_BRACKET);

        String callType = calleeFullMethod.substring(indexCalleeLeftBracket + 1, indexCalleeRightBracket);
        String finalCalleeFullMethod = calleeFullMethod.substring(indexCalleeRightBracket + 1).trim();

        logger.debug("\r\n[{}]\r\n[{}]\r\n[{}]\r\n[{}]\r\n[{}]", callType, callerFullMethod, calleeFullMethod, finalCalleeFullMethod, callerLineNum);

        // 根据类名前缀判断是否需要处理
        if (confInfo.isInputIgnoreOtherPackage() && (!isAllowedClassPrefix(callerFullMethod) || !isAllowedClassPrefix(finalCalleeFullMethod))) {
            return true;
        }

        String callerMethodHash = JACGUtil.genHashWithLen(callerFullMethod);
        String calleeMethodHash = JACGUtil.genHashWithLen(finalCalleeFullMethod);

        if (callerMethodHash.equals(calleeMethodHash)) {
            // 对于递归调用，不写入数据库，防止查询时出现死循环
            logger.info("递归调用不写入数据库 {}", callerFullMethod);
            return true;
        }

        String callerMethodName = JACGUtil.getOnlyMethodName(callerFullMethod);
        String calleeMethodName = JACGUtil.getOnlyMethodName(finalCalleeFullMethod);

        String callerFullClassName = JACGUtil.getFullClassNameFromMethod(callerFullMethod);
        String calleeFullClassName = JACGUtil.getFullClassNameFromMethod(finalCalleeFullMethod);

        String callerFullOrSimpleClassName = getFullOrSimpleClassName(callerFullClassName);
        String calleeFullOrSimpleClassName = getFullOrSimpleClassName(calleeFullClassName);

        MethodCallEntity methodCallEntity = new MethodCallEntity();
        methodCallEntity.setId(Integer.valueOf(callIdStr));
        methodCallEntity.setCallType(callType);
        methodCallEntity.setEnabled(JACGConstants.ENABLED);
        methodCallEntity.setCallerJarNum(callerJarNum);
        methodCallEntity.setCallerMethodHash(callerMethodHash);
        methodCallEntity.setCallerFullMethod(callerFullMethod);
        methodCallEntity.setCallerMethodName(callerMethodName);
        methodCallEntity.setCallerFullClassName(callerFullClassName);
        methodCallEntity.setCallerFullOrSimpleClassName(callerFullOrSimpleClassName);
        methodCallEntity.setCallerLineNum(callerLineNum);
        methodCallEntity.setCalleeMethodHash(calleeMethodHash);
        methodCallEntity.setFinalCalleeFullMethod(finalCalleeFullMethod);
        methodCallEntity.setCalleeMethodName(calleeMethodName);
        methodCallEntity.setCalleeFullClassName(calleeFullClassName);
        methodCallEntity.setCalleeFullOrSimpleClassName(calleeFullOrSimpleClassName);

        methodCallList.add(methodCallEntity);

        if (methodCallList.size() >= JACGConstants.BATCH_SIZE) {
            writeMethodCall2Db();
        }

        return true;
    }

    private void writeMethodCall2Db() {
        if (methodCallList.isEmpty()) {
            return;
        }

        List<Object[]> tmpMethodCallList = genWriteDBList();
        methodCallList.clear();

        // 等待直到允许任务执行
        wait4TPEExecute();

        threadPoolExecutor.execute(() -> {
            logger.info("写入数据库，方法调用关系表 {}", tmpMethodCallList.size());

            String sqlKey = JACGConstants.SQL_KEY_INSERT_METHOD_CALL;
            String sql = sqlCacheMap.get(sqlKey);
            if (sql == null) {
                sql = genAndCacheInsertSql(sqlKey,
                        false,
                        JACGConstants.TABLE_PREFIX_METHOD_CALL,
                        JACGConstants.TABLE_COLUMNS_METHOD_CALL);
            }

            if (!dbOperator.batchInsert(sql, tmpMethodCallList)) {
                someTaskFail = true;
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

    /**
     * 根据完整类名获取对应的类名
     * 若当前简单类名存在1个以上，则返回完整类名
     * 若当前简单类名只有1个，则返回简单类名
     *
     * @param fullClassName 完整类名信息
     * @return 完整类名或简单类名
     */
    private String getFullOrSimpleClassName(String fullClassName) {
        String simpleClassName = JACGUtil.getSimpleClassNameFromFull(fullClassName);
        if (duplicateClassNameSet.contains(simpleClassName)) {
            return fullClassName;
        }
        return simpleClassName;
    }

    private String genAndCacheInsertSql(String key, boolean replaceInto, String tableName, String[] columns) {
        String sql = sqlCacheMap.get(key);
        if (sql == null) {
            sql = replaceInto ? "replace into " : "insert into ";
            sql = sql + tableName + confInfo.getAppName() + SqlUtil.genColumnString(columns) +
                    " values " + SqlUtil.genQuestionString(columns.length);
            cacheSql(key, sql);
        }
        return sql;
    }

    private List<Object[]> genWriteDBList() {
        List<Object[]> tmpMethodCallList = new ArrayList<>(methodCallList.size());
        for (MethodCallEntity methodCallEntity : methodCallList) {
            Object[] object = new Object[]{
                    methodCallEntity.getId(),
                    methodCallEntity.getCallType(),
                    methodCallEntity.getEnabled(),
                    methodCallEntity.getCallerJarNum(),
                    methodCallEntity.getCallerMethodHash(),
                    methodCallEntity.getCallerFullMethod(),
                    methodCallEntity.getCallerMethodName(),
                    methodCallEntity.getCallerFullClassName(),
                    methodCallEntity.getCallerFullOrSimpleClassName(),
                    methodCallEntity.getCallerLineNum(),
                    methodCallEntity.getCalleeMethodHash(),
                    methodCallEntity.getFinalCalleeFullMethod(),
                    methodCallEntity.getCalleeMethodName(),
                    methodCallEntity.getCalleeFullClassName(),
                    methodCallEntity.getCalleeFullOrSimpleClassName()
            };
            tmpMethodCallList.add(object);
        }
        return tmpMethodCallList;
    }

    // 显示H2数据库JDBC URL
    private void printH2JdbcUrl() {
        String h2DbFilePath = FileUtil.getCanonicalPath(getH2DbFile());
        if (h2DbFilePath == null) {
            return;
        }
        String h2DbFilePathWithoutExt = h2DbFilePath.substring(0, h2DbFilePath.length() - JACGConstants.H2_FILE_EXT.length());
        logger.info("可用于连接H2数据库的JDBC URL:\n{}{}", JACGConstants.H2_PROTOCOL, h2DbFilePathWithoutExt);
    }

    public boolean isJavaCGRecordAll() {
        return javaCGRecordAll;
    }

    public void setJavaCGRecordAll(boolean javaCGRecordAll) {
        this.javaCGRecordAll = javaCGRecordAll;
    }
}

