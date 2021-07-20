package com.adrninistrator.jacg.runner;

import com.adrninistrator.jacg.common.Constants;
import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.dto.MethodCallEntity;
import com.adrninistrator.jacg.runner.base.AbstractRunner;
import com.adrninistrator.jacg.util.CommonUtil;
import com.adrninistrator.jacg.util.FileUtil;
import com.adrninistrator.jacg.util.SqlUtil;
import gr.gousiosg.javacg.stat.JCallGraph;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.*;

//import gr.gousiosg.javacg.stat.JCallGraph;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description: 将通过java-callgraph生成的直接调用关系文件写入数据库
 */

public class RunnerWriteDb extends AbstractRunner {

    private static final Logger logger = LoggerFactory.getLogger(RunnerWriteDb.class);

    // 当类名为以下前缀时，才处理
    private Set<String> allowedClassPrefixSet;

    // 已写入数据库的完整类名
    private Map<String, Boolean> fullClassNameMap = new HashMap<>(Constants.BATCH_SIZE);

    // 完整类名缓存
    private List<String> fullClassNameList = new ArrayList<>(Constants.BATCH_SIZE);

    // 类名相同但包名不同的类名
    private Set<String> duplicateClassNameSet = new HashSet<>();

    // 方法注解缓存
    private List<Pair<String, String>> methodAnnotationList = new ArrayList<>(Constants.BATCH_SIZE);

    // 方法调用缓存
    private List<MethodCallEntity> methodCallList = new ArrayList<>(Constants.BATCH_SIZE);

    // 记录是否读取到文件
    private boolean readFileFlag;

    // 记录是否有写数据库
    private boolean writeDbFlag;

    // 当前处理的调用关系序号
    private int id = Constants.METHOD_CALL_ID_START;

    static {
        runner = new RunnerWriteDb();
    }

    @Override
    public boolean init() {
        if (confInfo.getDbDriverName().contains(Constants.MYSQL_FLAG) &&
                !confInfo.getDbUrl().contains(Constants.MYSQL_REWRITEBATCHEDSTATEMENTS)) {
            logger.info("使用MYSQL时，请在{}参数指定{}", Constants.KEY_DB_URL, Constants.MYSQL_REWRITEBATCHEDSTATEMENTS);
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
        // 判断是否需要调用java-callgraph生成jar包的方法调用关系
        if (!callJavaCallGraph()) {
            return;
        }

        // 创建数据库表
        if (!createTables()) {
            return;
        }

        // 清理数据库表
        if (!truncateTables()) {
            return;
        }

        // 读取通过java-callgraph生成的直接调用关系文件，处理类名
        if (!handleClassCall()) {
            return;
        }

        if (!readFileFlag) {
            if (confInfo.isInputIgnoreOtherPackage()) {
                logger.warn("未从文件读取到内容，请检查文件 {} ，以及配置文件指定的包名 {}", confInfo.getCallGraphInputFile(), Constants.FILE_IN_ALLOWED_CLASS_PREFIX);
            } else {
                logger.warn("未从文件读取到内容，请检查文件 {}", confInfo.getCallGraphInputFile());
            }
        }
        if (!writeDbFlag) {
            logger.warn("未向数据库写入数据，请检查文件内容 {}", confInfo.getCallGraphInputFile());
        }

        // 查找类名相同但包名不同的类
        if (!findDuplicateClass()) {
            return;
        }

        // 读取方法注解信息
        if (!handleMethodAnnotation()) {
            return;
        }

        // 创建线程
        createThreadPoolExecutor();

        // 读取通过java-callgraph生成的直接调用关系文件，处理方法调用
        handleMethodCall();

        // 等待直到任务执行完毕
        wait4TPEDone();
    }

    // 判断是否需要调用java-callgraph生成jar包的方法调用关系
    private boolean callJavaCallGraph() {
        if (StringUtils.isBlank(confInfo.getCallGraphJarList())) {
            return true;
        }

        logger.info("尝试调用java-callgraph生成jar包的方法调用关系 {}", confInfo.getCallGraphJarList());

        String[] array = confInfo.getCallGraphJarList().split(Constants.FLAG_SPACE);
        for (String jarName : array) {
            File jarFile = new File(jarName);
            if (!jarFile.exists() || !jarFile.isFile()) {
                logger.error("文件不存在或不是文件 {}", jarName);
                return false;
            }
        }

        System.setProperty(Constants.JAVA_CALL_GRAPH_FLAG_OUT_FILE, confInfo.getCallGraphInputFile());

        // 调用java-callgraph
        boolean success = JCallGraph.run(array);
        if (!success) {
            logger.error("调用java-callgraph生成jar包的方法调用关系失败");
        }
        return success;
    }

    // 创建数据库表
    private boolean createTables() {
        String sqlClassName = readCreateTableSql(Constants.DIR_SQL + File.separator + Constants.FILE_SQL_CLASS_NAME);
        String sqlMethodAnnotation = readCreateTableSql(Constants.DIR_SQL + File.separator + Constants.FILE_SQL_METHOD_ANNOTATION);
        String sqlMethodCall = readCreateTableSql(Constants.DIR_SQL + File.separator + Constants.FILE_SQL_METHOD_CALL);

        if (!dbOperator.createTable(sqlClassName) ||
                !dbOperator.createTable(sqlMethodAnnotation) ||
                !dbOperator.createTable(sqlMethodCall)) {
            return false;
        }

        return true;
    }

    private String readCreateTableSql(String sqlFilePath) {
        String sql = FileUtil.readFile2String(sqlFilePath);
        if (StringUtils.isBlank(sql)) {
            logger.error("文件内容为空 {}", sqlFilePath);
        }

        sql = sql.replace(Constants.APPNAME_IN_SQL, confInfo.getAppName());

        logger.info("建表sql: {}", sql);
        return sql;
    }

    // 清理数据库表
    private boolean truncateTables() {
        if (!dbOperator.truncateTable(Constants.TABLE_PREFIX_CLASS_NAME + confInfo.getAppName()) ||
                !dbOperator.truncateTable(Constants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName()) ||
                !dbOperator.truncateTable(Constants.TABLE_PREFIX_METHOD_CALL + confInfo.getAppName())) {
            return false;
        }
        return true;
    }

    // 读取其他配置文件
    private boolean readOtherConfig() {
        if (confInfo.isInputIgnoreOtherPackage()) {
            String allowedClassPrefixFile = Constants.DIR_CONFIG + File.separator + Constants.FILE_IN_ALLOWED_CLASS_PREFIX;
            allowedClassPrefixSet = FileUtil.readFile2Set(allowedClassPrefixFile);
            if (CommonUtil.isCollectionEmpty(allowedClassPrefixSet)) {
                logger.error("读取文件不存在或内容为空 {}", allowedClassPrefixFile);
                return false;
            }
        }
        return true;
    }

    // 读取通过java-callgraph生成的直接调用关系文件，处理类名
    private boolean handleClassCall() {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(confInfo.getCallGraphInputFile())))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                if (!readFileFlag) {
                    readFileFlag = true;
                }

                if (line.startsWith(Constants.FILE_KEY_CLASS_PREFIX)) {
                    // 处理一个类名
                    handleOneClassCall(line);
                }
            }

            // 结束前将缓存剩余数据写入数据库
            writeClassName2Db();

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 查找类名相同但包名不同的类
    private boolean findDuplicateClass() {
        String sql = sqlCacheMap.get(Constants.SQL_KEY_CN_QUERY_DUPLICATE_CLASS);
        if (sql == null) {
            sql = "select " + DC.CN_SIMPLE_NAME + " from " + Constants.TABLE_PREFIX_CLASS_NAME + confInfo.getAppName() +
                    " group by " + DC.CN_SIMPLE_NAME + " having count(" + DC.CN_SIMPLE_NAME + ") > 1";
            cacheSql(Constants.SQL_KEY_CN_QUERY_DUPLICATE_CLASS, sql);
        }

        List<Object> list = dbOperator.queryListOneObject(sql, null);
        if (list == null) {
            return false;
        }

        if (!CommonUtil.isCollectionEmpty(list)) {
            for (Object object : list) {
                duplicateClassNameSet.add((String) object);
            }
        }
        return true;
    }

    // 处理一个类名
    private void handleOneClassCall(String data) {
        int indexBlank = data.indexOf(Constants.FLAG_SPACE);

        String callerFullClassName = data.substring(Constants.FILE_KEY_PREFIX_LENGTH, indexBlank).trim();
        String calleeFullClassName = data.substring(indexBlank + 1).trim();

        logger.debug("[{}] [{}]", callerFullClassName, calleeFullClassName);

        handleClassName(callerFullClassName);
        handleClassName(calleeFullClassName);
    }

    private void handleClassName(String fullClassName) {
        // 根据类名前缀判断是否需要处理
        if (confInfo.isInputIgnoreOtherPackage() && !isAllowedClassPrefix(fullClassName)) {
            return;
        }

        // 通过java-callgraph生成的直接类引用关系存在重复，进行去重
        if (fullClassNameMap.putIfAbsent(fullClassName, Boolean.TRUE) == null) {
            fullClassNameList.add(fullClassName);

            if (fullClassNameList.size() >= Constants.BATCH_SIZE) {
                writeClassName2Db();
            }
        }
    }

    private void writeClassName2Db() {
        if (fullClassNameList.isEmpty()) {
            return;
        }

        logger.info("写入数据库，保存类名信息表 {}", fullClassNameList.size());

        if (!writeDbFlag) {
            writeDbFlag = true;
        }

        String sql = sqlCacheMap.get(Constants.SQL_KEY_INSERT_CLASS_NAME);
        if (sql == null) {
            sql = genAndCacheInsertSql(Constants.SQL_KEY_INSERT_CLASS_NAME,
                    Constants.TABLE_PREFIX_CLASS_NAME,
                    Constants.TABLE_COLUMNS_CLASS_NAME);
        }

        List<Object[]> objectList = new ArrayList<>(fullClassNameList.size());
        for (String fullClassName : fullClassNameList) {
            String simpleClassName = CommonUtil.getSimpleClassNameFromFull(fullClassName);

            Object[] object = new Object[]{fullClassName, simpleClassName};
            objectList.add(object);
        }

        dbOperator.batchInsert(sql, objectList);

        fullClassNameList.clear();
    }

    // 读取方法注解信息
    private boolean handleMethodAnnotation() {
        String annotationInfoFilePath = confInfo.getCallGraphInputFile() + Constants.FILE_IN_ANNOTATION_TAIL;
        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(annotationInfoFilePath)))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                // 处理一个方法注解
                handleOneMethodAnnotation(line);
            }

            // 结束前将缓存剩余数据写入数据库
            writeMethodAnnotation2Db();

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理一个方法注解
    private void handleOneMethodAnnotation(String data) {

        String[] array = data.split(Constants.FLAG_SPACE);
        String fullMethod = array[0];

        // 根据类名前缀判断是否需要处理
        if (confInfo.isInputIgnoreOtherPackage() && !isAllowedClassPrefix(fullMethod)) {
            return;
        }

        String annotation = array[1];

        logger.debug("[{}] [{}]", fullMethod, annotation);

        Pair<String, String> pair = new ImmutablePair<>(fullMethod, annotation);
        methodAnnotationList.add(pair);

        if (methodAnnotationList.size() >= Constants.BATCH_SIZE) {
            writeMethodAnnotation2Db();
        }
    }

    private void writeMethodAnnotation2Db() {
        if (methodAnnotationList.isEmpty()) {
            return;
        }

        logger.info("写入数据库，保存方法注解信息表 {}", methodAnnotationList.size());

        if (!writeDbFlag) {
            writeDbFlag = true;
        }

        String sql = sqlCacheMap.get(Constants.SQL_KEY_INSERT_METHOD_ANNOTATION);
        if (sql == null) {
            sql = genAndCacheInsertSql(Constants.SQL_KEY_INSERT_METHOD_ANNOTATION,
                    Constants.TABLE_PREFIX_METHOD_ANNOTATION,
                    Constants.TABLE_COLUMNS_METHOD_ANNOTATION);
        }

        List<Object[]> objectList = new ArrayList<>(methodAnnotationList.size());
        for (Pair<String, String> pair : methodAnnotationList) {
            String fullMethod = pair.getLeft();
            String annotation = pair.getRight();
            String methodHash = CommonUtil.genHashWithLen(fullMethod);

            Object[] object = new Object[]{methodHash, annotation, fullMethod};
            objectList.add(object);
        }

        dbOperator.batchInsert(sql, objectList);

        methodAnnotationList.clear();
    }


    // 读取通过java-callgraph生成的直接调用关系文件，处理方法调用
    private void handleMethodCall() {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(confInfo.getCallGraphInputFile())))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (StringUtils.isBlank(line)) {
                    continue;
                }

                if (line.startsWith(Constants.FILE_KEY_METHOD_PREFIX)) {
                    // 处理一条方法调用
                    handleOneMethodCall(line);
                }
            }

            // 结束前将缓存剩余数据写入数据库
            writeMethodCall2Db();
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    // 处理一条方法调用
    private void handleOneMethodCall(String data) {
        int indexBlank = data.indexOf(Constants.FLAG_SPACE);

        String callerFullMethod = data.substring(Constants.FILE_KEY_PREFIX_LENGTH, indexBlank).trim();
        String calleeFullMethod = data.substring(indexBlank + 1);

        int indexCalleeLeftBracket = calleeFullMethod.indexOf(Constants.FLAG_LEFT_BRACKET);
        int indexCalleeRightBracket = calleeFullMethod.indexOf(Constants.FLAG_RIGHT_BRACKET);

        String callType = calleeFullMethod.substring(indexCalleeLeftBracket + 1, indexCalleeRightBracket);
        String finalCalleeFullMethod = calleeFullMethod.substring(indexCalleeRightBracket + 1).trim();

        logger.debug("\r\n[{}]\r\n[{}]\r\n[{}]\r\n[{}]", callType, callerFullMethod, calleeFullMethod, finalCalleeFullMethod);

        // 根据类名前缀判断是否需要处理
        if (confInfo.isInputIgnoreOtherPackage() && (!isAllowedClassPrefix(callerFullMethod) || !isAllowedClassPrefix(finalCalleeFullMethod))) {
            return;
        }

        String callerMethodHash = CommonUtil.genHashWithLen(callerFullMethod);
        String calleeMethodHash = CommonUtil.genHashWithLen(finalCalleeFullMethod);

        if (callerMethodHash.equals(calleeMethodHash)) {
            // 对于递归调用，不写入数据库，防止查询时出现死循环
            logger.info("递归调用不写入数据库 {}", callerFullMethod);
            return;
        }

        String callerMethodName = CommonUtil.getOnlyMethodName(callerFullMethod);
        String calleeMethodName = CommonUtil.getOnlyMethodName(finalCalleeFullMethod);

        String callerFullClassName = CommonUtil.getFullClassNameFromMethod(callerFullMethod);
        String calleeFullClassName = CommonUtil.getFullClassNameFromMethod(finalCalleeFullMethod);

        String callerFullOrSimpleClassName = getFullOrSimpleClassName(callerFullClassName);
        String calleeFullOrSimpleClassName = getFullOrSimpleClassName(calleeFullClassName);

        MethodCallEntity methodCallEntity = new MethodCallEntity();
        methodCallEntity.setId(Integer.valueOf(++id));
        methodCallEntity.setCallType(callType);
        methodCallEntity.setCallerMethodHash(callerMethodHash);
        methodCallEntity.setCallerFullMethod(callerFullMethod);
        methodCallEntity.setCallerMethodName(callerMethodName);
        methodCallEntity.setCallerFullClassName(callerFullClassName);
        methodCallEntity.setCallerFullOrSimpleClassName(callerFullOrSimpleClassName);
        methodCallEntity.setCalleeMethodHash(calleeMethodHash);
        methodCallEntity.setFinalCalleeFullMethod(finalCalleeFullMethod);
        methodCallEntity.setCalleeMethodName(calleeMethodName);
        methodCallEntity.setCalleeFullClassName(calleeFullClassName);
        methodCallEntity.setCalleeFullOrSimpleClassName(calleeFullOrSimpleClassName);

        methodCallList.add(methodCallEntity);

        if (methodCallList.size() >= Constants.BATCH_SIZE) {
            writeMethodCall2Db();
        }
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

            String sql = sqlCacheMap.get(Constants.SQL_KEY_INSERT_METHOD_CALL);
            if (sql == null) {
                sql = genAndCacheInsertSql(Constants.SQL_KEY_INSERT_METHOD_CALL,
                        Constants.TABLE_PREFIX_METHOD_CALL,
                        Constants.TABLE_COLUMNS_METHOD_CALL);
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
     * @return true：需要处理，false：忽略
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
        String simpleClassName = CommonUtil.getSimpleClassNameFromFull(fullClassName);
        if (duplicateClassNameSet.contains(simpleClassName)) {
            return fullClassName;
        }
        return simpleClassName;
    }

    private String genAndCacheInsertSql(String key, String tableName, String[] columns) {
        String sql = sqlCacheMap.get(key);
        if (sql == null) {
            sql = "insert ignore into " + tableName + confInfo.getAppName() + SqlUtil.genColumnString(columns) +
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
                    methodCallEntity.getCallerMethodHash(),
                    methodCallEntity.getCallerFullMethod(),
                    methodCallEntity.getCallerMethodName(),
                    methodCallEntity.getCallerFullClassName(),
                    methodCallEntity.getCallerFullOrSimpleClassName(),
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
}

