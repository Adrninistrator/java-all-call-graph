package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4LambdaMethodInfo;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodAnnotation;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author adrninistrator
 * @date 2022/8/23
 * @description: 数据库公共操作封装对象
 */
public class DbOperWrapper {
    private static final Logger logger = LoggerFactory.getLogger(DbOperWrapper.class);

    private static final AtomicInteger ATOMIC_INTEGER = new AtomicInteger(0);

    // 预编译SQL语句缓存，不能使用静态字段，否则多个任务之间会相互影响
    private final Map<String, String> sqlCacheMap = new ConcurrentHashMap<>();

    // 类名相同但包名不同的类名
    private Set<String> duplicateSimpleClassNameSet = null;

    private final DbOperator dbOperator;

    private final String appName;

    private final String objSeq;

    public DbOperWrapper(DbOperator dbOperator) {
        this.dbOperator = dbOperator;
        this.appName = dbOperator.getAppName();

        objSeq = String.valueOf(ATOMIC_INTEGER.incrementAndGet());
        logger.info("objSeq [{}]", objSeq);
    }


    /**
     * 完成需要使用的基础配置的初始化
     * 通过返回对象获取相关配置对象方式：
     * 通过 DbOperWrapper.getDbOperator()可获取 DbOperator
     *
     * @param configureWrapper
     * @param currentSimpleName 当前对应的简单类名
     * @return
     */
    public static DbOperWrapper genInstance(ConfigureWrapper configureWrapper, String currentSimpleName) {
        DbOperator dbOperator = DbOperator.genInstance(configureWrapper, currentSimpleName);
        if (dbOperator == null) {
            throw new JavaCGRuntimeException("数据库初始化失败");
        }
        return new DbOperWrapper(dbOperator);
    }

    private String genSqlKey(String sqlKey, int num) {
        if (num == 0) {
            return sqlKey;
        }
        return sqlKey + JACGConstants.FLAG_AT + num;
    }

    private String getCachedSql(String sqlKey, int num) {
        return sqlCacheMap.get(genSqlKey(sqlKey, num));
    }

    /**
     * 获取缓存的sql语句，参数数量可变
     *
     * @param sqlKeyEnum
     * @param num
     * @return
     */
    public String getCachedSql(SqlKeyEnum sqlKeyEnum, int num) {
        return getCachedSql(String.valueOf(sqlKeyEnum.ordinal()), num);
    }

    private String getCachedSql(String sqlKey) {
        return getCachedSql(sqlKey, 0);
    }

    /**
     * 获取缓存的sql语句，参数数量固定
     *
     * @param sqlKeyEnum
     * @return
     */
    public String getCachedSql(SqlKeyEnum sqlKeyEnum) {
        return getCachedSql(String.valueOf(sqlKeyEnum.ordinal()));
    }

    private String cacheSql(String sqlKey, String sql, String sqlKey4Print, int num) {
        // 根据sql语句的key与参数数量，生成最终的key
        String finalSqlKey = genSqlKey(sqlKey, num);

        // 替换sql语句中的appName
        String finalSql = JACGSqlUtil.replaceAppNameInSql(sql, appName);
        if (sqlCacheMap.putIfAbsent(finalSqlKey, finalSql) == null) {
            // 假如有指定用于在日志中打印的key，则在日志中打印出来
            logger.info("[{}] cache sql: [{} {}] [{}]", objSeq, finalSqlKey, sqlKey4Print, finalSql);
        }
        return finalSql;
    }

    /**
     * 缓存并格式化sql，参数数量可变
     *
     * @param sqlKeyEnum
     * @param sql
     * @param num
     */
    public String cacheSql(SqlKeyEnum sqlKeyEnum, String sql, int num) {
        return cacheSql(String.valueOf(sqlKeyEnum.ordinal()), sql, sqlKeyEnum.name(), num);
    }

    private String cacheSql(String sqlKey, String sql, String key4Print) {
        return cacheSql(sqlKey, sql, key4Print, 0);
    }

    /**
     * 缓存并格式化sql，参数数量固定
     *
     * @param sqlKeyEnum
     * @param sql
     */
    public String cacheSql(SqlKeyEnum sqlKeyEnum, String sql) {
        return cacheSql(String.valueOf(sqlKeyEnum.ordinal()), sql, sqlKeyEnum.name());
    }

    /**
     * 格式化sql语句，适用于不需要缓存的sql语句
     *
     * @param sql 格式化前的sql语句
     * @return 格式化后的sql语句
     */
    public String formatSql(String sql) {
        // 替换sql语句中的appName
        String finalSql = JACGSqlUtil.replaceAppNameInSql(sql, appName);
        logger.info("[{}] format sql: [{}]", objSeq, finalSql);
        return finalSql;
    }

    /**
     * 生成用于插入数据库的sql语句并缓存
     *
     * @param dbTableInfoEnum
     * @param dbInsertMode
     * @return
     */
    public String genAndCacheInsertSql(DbTableInfoEnum dbTableInfoEnum, DbInsertMode dbInsertMode) {
        String key = dbTableInfoEnum.getSqlKey();
        String[] columns = dbTableInfoEnum.getColumns();
        String sql = getCachedSql(key);
        if (sql == null) {
            sql = dbInsertMode.getMode() + dbTableInfoEnum.getTableName() + JACGSqlUtil.genColumnString(columns) + " values " + JACGSqlUtil.genQuestionString(columns.length);
            sql = cacheSql(key, sql, dbTableInfoEnum.getSqlKey4Print());
        }
        return sql;
    }

    /**
     * 从方法注解表，查询带有指定注解的完整方法及方法HASH，结果会去重
     *
     * @param annotationClassNames 注解类名
     * @return
     */
    @SuppressWarnings("all")
    public List<WriteDbData4MethodAnnotation> getMethodsAndHashWithAnnotations(String... annotationClassNames) {
        if (ArrayUtils.isEmpty(annotationClassNames)) {
            return Collections.emptyList();
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_FMAH_WITH_ANNOTATIONS;
        String sql = getCachedSql(sqlKeyEnum, annotationClassNames.length);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.MA_FULL_METHOD, DC.MA_METHOD_HASH) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            sql = cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
        }
        // IDEA的提示忽略 Confusing argument 'xxx', unclear if a varargs or non-varargs call is desired
        return dbOperator.queryList(sql, WriteDbData4MethodAnnotation.class, annotationClassNames);
    }

    /**
     * 从方法注解表，查询带有指定注解的，且在指定类中的完整方法及方法HASH
     *
     * @param className            完整类名
     * @param annotationClassNames 注解类名
     * @return
     */
    public List<WriteDbData4MethodAnnotation> getMethodsAndHashWithAnnotationsOfClass(String className, String... annotationClassNames) {
        if (className == null || ArrayUtils.isEmpty(annotationClassNames)) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_FMAH_WITH_ANNOTATIONS_OF_CLASS;
        String sql = getCachedSql(sqlKeyEnum, annotationClassNames.length);
        if (sql == null) {
            // 指定完整方法需要以[完整类名]:开关，只查询指定类中的方法
            sql = "select " + JACGSqlUtil.joinColumns(DC.MA_FULL_METHOD, DC.MA_METHOD_HASH) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length) +
                    " and " + DC.MA_FULL_METHOD + " like concat(?, '%')";
            sql = cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
        }

        List<String> argList = new ArrayList<>(annotationClassNames.length + 2);
        argList.addAll(Arrays.asList(annotationClassNames));
        argList.add(className + JavaCGConstants.FLAG_COLON);

        return dbOperator.queryList(sql, WriteDbData4MethodAnnotation.class, argList.toArray());
    }

    /**
     * 从方法注解表，查询带有指定注解的完整方法
     *
     * @param annotationClassNames 注解类名数组
     * @return
     */
    @SuppressWarnings("all")
    public List<String> getMethodsWithAnnotations(String... annotationClassNames) {
        if (ArrayUtils.isEmpty(annotationClassNames)) {
            return Collections.emptyList();
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_FULL_METHOD_WITH_ANNOTATIONS;
        String sql = getCachedSql(sqlKeyEnum, annotationClassNames.length);
        if (sql == null) {
            sql = "select " + DC.MA_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            sql = cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
        }
        // IDEA的提示忽略 Confusing argument 'xxx', unclear if a varargs or non-varargs call is desired
        return dbOperator.queryListOneColumn(sql, String.class, annotationClassNames);
    }

    /**
     * 根据调用者完整方法HASH+长度，从方法调用表获取对应的完整方法
     *
     * @param methodHash 完整方法HASH+长度
     * @return
     */
    public String getCallerFullMethodByHash(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLER_FULL_METHOD_BY_HASH;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_METHOD_HASH + " = ?" +
                    " limit 1";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        String callerFullMethod = dbOperator.queryObjectOneColumn(sql, String.class, methodHash);
        if (callerFullMethod == null) {
            logger.error("根据调用者完整方法HASH+长度未找到完整方法 {}", methodHash);
        }
        return callerFullMethod;
    }

    /**
     * 根据被调用者完整方法HASH+长度，从方法调用表获取对应的完整方法
     *
     * @param methodHash 完整方法HASH+长度
     * @return
     */
    public String getCalleeFullMethodByHash(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLEE_FULL_METHOD_BY_HASH;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLEE_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " limit 1";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        String calleeFullMethod = dbOperator.queryObjectOneColumn(sql, String.class, methodHash);
        if (calleeFullMethod == null) {
            logger.error("根据被调用者完整方法HASH+长度未找到完整方法 {}", methodHash);
        }
        return calleeFullMethod;
    }

    /**
     * 根据方法调用ID，从方法调用表获取对应的完整方法
     *
     * @param methodCallId 方法调用ID
     * @return
     */
    public String getCalleeFullMethodById(int methodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLEE_FULL_METHOD_BY_ID;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLEE_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALL_ID + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        String calleeFullMethod = dbOperator.queryObjectOneColumn(sql, String.class, methodCallId);
        if (calleeFullMethod == null) {
            logger.error("根据方法调用ID未找到完整方法 {}", methodCallId);
        }
        return calleeFullMethod;
    }

    /**
     * 根据被调用完整方法HASH+长度，与被调用对象类型，查询对应的方法调用信息
     *
     * @param calleeMethodHash
     * @param calleeObjType
     * @return
     */
    public List<WriteDbData4MethodCall> getMethodCallByCalleeHashObjType(String calleeMethodHash, String calleeObjType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_METHOD_CALL_BY_CALLEE_HASH_OBJ_TYPE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALLER_FULL_METHOD, DC.MC_CALLER_LINE_NUMBER, DC.MC_CALLEE_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " and " + DC.MC_CALLEE_OBJ_TYPE + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, calleeMethodHash, calleeObjType);
    }

    /**
     * 查找类名相同但包名不同的类
     *
     * @return
     */
    public boolean findDuplicateClass() {
        logger.info("查找类名相同但包名不同的类");
        duplicateSimpleClassNameSet = new HashSet<>();

        // 查找类名与唯一类名相同，且唯一类名中包含.的唯一类名
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_DUPLICATE_CLASS;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CN_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " where " + DC.CN_DUPLICATE_CLASS + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        List<String> list = dbOperator.queryListOneColumn(sql, String.class, JavaCGYesNoEnum.YES.getIntValue());
        if (list == null) {
            duplicateSimpleClassNameSet = null;
            return false;
        }
        if (list.isEmpty()) {
            return true;
        }

        for (String simpleClassName : list) {
            String duplicateSimpleClassName = JACGClassMethodUtil.getSimpleClassNameFromFull(simpleClassName);
            duplicateSimpleClassNameSet.add(duplicateSimpleClassName);
        }
        logger.info("找到类名相同但包名不同的类 {}", StringUtils.join(duplicateSimpleClassNameSet, " "));
        return true;
    }

    /**
     * 将类名表中的同名类更新为使用完整类名之前，查找类名相同但包名不同的类
     *
     * @return
     */
    public Set<String> findDuplicateClassBeforeUpdate() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_DUPLICATE_CLASS_BEFORE_UPDATE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CN_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " group by " + DC.CN_SIMPLE_CLASS_NAME +
                    " having count(" + DC.CN_SIMPLE_CLASS_NAME + ") > 1";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        List<String> list = dbOperator.queryListOneColumn(sql, String.class);
        if (list == null) {
            return Collections.emptySet();
        }
        return new HashSet<>(list);
    }

    /**
     * 将类名表中的同名类更新为使用完整类名，并记录同名类
     *
     * @return
     */
    public boolean updateAllSimpleName2Full() {
        Set<String> foundDuplicateSimpleClassNameSet = findDuplicateClassBeforeUpdate();
        if (foundDuplicateSimpleClassNameSet.isEmpty()) {
            logger.info("不存在类名相同但包名不同的类");
            return true;
        }

        logger.info("找到类名相同但包名不同的类 {}", StringUtils.join(foundDuplicateSimpleClassNameSet, " "));
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_UPDATE_SIMPLE_2_FULL;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "update " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " set " + DC.CN_SIMPLE_CLASS_NAME + " = " + DC.CN_CLASS_NAME + "," + DC.CN_DUPLICATE_CLASS + " = ?" +
                    " where " + DC.CN_SIMPLE_CLASS_NAME + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        for (String duplicateClassName : foundDuplicateSimpleClassNameSet) {
            // 将class_name_表的simple_name更新为full_name
            if (dbOperator.update(sql, JavaCGYesNoEnum.YES.getIntValue(), duplicateClassName) == null) {
                return false;
            }
        }

        duplicateSimpleClassNameSet = foundDuplicateSimpleClassNameSet;
        return true;
    }

    /**
     * 根据完整类名获取对应的类名
     * 若当前简单类名存在1个以上，则返回完整类名
     * 若当前简单类名只有1个，则返回简单类名
     *
     * @param className 完整类名信息
     * @return 完整类名或简单类名
     */
    public String getSimpleClassName(String className) {
        if (duplicateSimpleClassNameSet == null && !findDuplicateClass()) {
            throw new JavaCGRuntimeException("查询同名类失败");
        }

        String simpleClassName = JACGClassMethodUtil.getSimpleClassNameFromFull(className);
        if (duplicateSimpleClassNameSet.contains(simpleClassName)) {
            return className;
        }
        return simpleClassName;
    }

    /**
     * 查询数据库方法调用表最大序号
     *
     * @return
     */
    public int getMaxMethodCallId() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_MAX_CALL_ID;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select max(" + DC.MC_CALL_ID + ") from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName();
            sql = cacheSql(sqlKeyEnum, sql);
        }

        Integer maxCallId = dbOperator.queryObjectOneColumn(sql, Integer.class);
        return maxCallId == null ? JACGConstants.MAX_METHOD_CALL_ID_ILLEGAL : maxCallId;
    }

    /**
     * 查询调用方法时包含指定方法调用业务功能数据的调用者完整方法
     *
     * @param dataTypeList    方法调用业务功能数据类型，可指定多个，关系为或（in xxx）
     * @param dataKeywordList 方法调用业务功能数据关键字，可指定多个，关系为且（like xxx and like xxx）
     * @return
     */
    public List<String> getCallerFullMethodWithBusinessData(List<String> dataTypeList, List<String> dataKeywordList) {
        if (dataKeywordList == null || dataTypeList == null) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.BD_QUERY_METHOD_BY_BUSINESS_DATA;
        String sql = getCachedSql(sqlKeyEnum, dataKeywordList.size());
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLER_FULL_METHOD + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALL_ID + " in " +
                    " (" +
                    " select " + DC.BD_CALL_ID +
                    " from " + DbTableInfoEnum.DTIE_BUSINESS_DATA.getTableName() +
                    " where " + DC.BD_DATA_TYPE + " in " + JACGSqlUtil.genQuestionString(dataTypeList.size()) +
                    StringUtils.repeat(" and " + DC.BD_DATA_VALUE + " like concat('%', ?, '%')", dataKeywordList.size()) +
                    ")";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        List<String> argList = new ArrayList<>(dataTypeList.size() + dataKeywordList.size());
        argList.addAll(dataTypeList);
        argList.addAll(dataKeywordList);

        return dbOperator.queryListOneColumn(sql, String.class, argList.toArray());
    }

    /**
     * 根据被调用方法HASH，判断方法调用表中是否存在常规的方法调用（排除ITF、SCC、CCS）
     *
     * @param calleeMethodHash 被调用方法HASH
     * @return false: 不存在 true: 存在
     */
    public boolean checkExistsNormalMethodCallByCalleeMethodHash(String calleeMethodHash) {
        if (calleeMethodHash == null) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CHECK_NORMAL_MC_BY_EE_HASH;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select count(" + DC.MC_CALLEE_METHOD_HASH + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ? and " +
                    DC.MC_CALL_TYPE + " not in (?, ?, ?)";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        Long count = dbOperator.queryObjectOneColumn(sql, Long.class,
                calleeMethodHash,
                JavaCGCallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS.getType(),
                JavaCGCallTypeEnum.CTE_SUPER_CALL_CHILD.getType(),
                JavaCGCallTypeEnum.CTE_CHILD_CALL_SUPER.getType());
        return count != null && count > 0;
    }

    /**
     * 查询方法调用中的调用方与被调用方，使用被调用方完整类名与方法
     *
     * @param calleeClassName  被调用方完整类名
     * @param calleeMethodName 被调用方方法
     * @return
     */
    public List<WriteDbData4MethodCall> getMethodCallByCalleeFullClassMethod(String calleeClassName, String calleeMethodName) {
        if (calleeClassName == null || calleeMethodName == null) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        String calleeSimpleClassName = getSimpleClassName(calleeClassName);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_MC_PAIR_BY_CALLEE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALLER_FULL_METHOD, DC.MC_CALLER_LINE_NUMBER, DC.MC_CALLEE_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLEE_METHOD_NAME + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, calleeSimpleClassName, calleeMethodName);
    }

    /**
     * 查询Lambda表达式中被调用方法信息
     *
     * @param methodCallId
     * @return
     */
    public WriteDbData4LambdaMethodInfo getLambdaCalleeInfo(int methodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.LMI_QUERY_CALLEE_INFO;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.LMI_LAMBDA_CALLEE_CLASS_NAME, DC.LMI_LAMBDA_CALLEE_METHOD_NAME) +
                    " from " + DbTableInfoEnum.DTIE_LAMBDA_METHOD_INFO.getTableName() +
                    " where " + DC.LMI_CALL_ID + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4LambdaMethodInfo.class, methodCallId);
    }

    /**
     * 根据完整方法HASH+长度，获取方法对应的标志
     *
     * @param methodHash
     * @return
     */
    public Integer getMethodFlags(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_FLAGS;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MI_ACCESS_FLAGS +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_METHOD_HASH + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryObjectOneColumn(sql, Integer.class, methodHash);
    }

    /**
     * 通过类名与方法名查询完整方法
     *
     * @param className
     * @param methodName
     * @return
     */
    public List<String> getMethodByClassMethod(String className, String methodName) {
        String simpleClassName = getSimpleClassName(className);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MI_QUERY_BY_CLASS_METHOD;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select " + DC.MI_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_INFO.getTableName() +
                    " where " + DC.MI_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MI_METHOD_NAME + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, simpleClassName, methodName);
    }

    /**
     * 根据被调用类名与方法名查询调用方信息
     *
     * @param calleeClassName
     * @param calleeMethodName
     * @return
     */
    public List<WriteDbData4MethodCall> getCallerInfoByCallee(String calleeClassName, String calleeMethodName) {
        String calleeSimpleClassName = getSimpleClassName(calleeClassName);
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_MC_PAIR_BY_CALLEE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALL_ID, DC.MC_CALLER_FULL_METHOD, DC.MC_CALL_FLAGS) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLEE_METHOD_NAME + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, calleeSimpleClassName, calleeMethodName);
    }

    public String getAppName() {
        return appName;
    }

    public DbOperator getDbOperator() {
        return dbOperator;
    }
}
