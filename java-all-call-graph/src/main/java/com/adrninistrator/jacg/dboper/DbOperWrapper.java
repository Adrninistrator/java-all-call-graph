package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.call_graph.CallGraphCallerInfo;
import com.adrninistrator.jacg.dto.method.ClassAndMethodName;
import com.adrninistrator.jacg.dto.method.MethodAndHash;
import com.adrninistrator.jacg.dto.method_call.MethodCallPair;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGCallTypeEnum;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
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
     * 生成用于缓存sql语句的key，参数数量可变
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
     * 生成用于缓存sql语句的key，参数数量固定
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
     * 缓存sql，参数数量可变
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
     * 缓存sql，参数数量固定
     *
     * @param sqlKeyEnum
     * @param sql
     */
    public String cacheSql(SqlKeyEnum sqlKeyEnum, String sql) {
        return cacheSql(String.valueOf(sqlKeyEnum.ordinal()), sql, sqlKeyEnum.name());
    }

    /**
     * 生成用于插入数据库的sql语句并缓存
     *
     * @param key
     * @param key4Print
     * @param dbInsertMode
     * @param tableName
     * @param columns
     * @return
     */
    public String genAndCacheInsertSql(String key, String key4Print, DbInsertMode dbInsertMode, String tableName, String[] columns) {
        String sql = getCachedSql(key);
        if (sql == null) {
            sql = dbInsertMode.getMode() + tableName + JACGSqlUtil.genColumnString(columns) + " values " + JACGSqlUtil.genQuestionString(columns.length);
            sql = cacheSql(key, sql, key4Print);
        }
        return sql;
    }

    /**
     * 从方法注解表，查询带有指定注解的完整方法及方法HASH，结果会去重
     *
     * @param annotationClassNames 注解类名
     * @return
     */
    public List<MethodAndHash> getMethodsAndHashWithAnnotations(String... annotationClassNames) {
        if (ArrayUtils.isEmpty(annotationClassNames)) {
            return null;
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_FMAH_WITH_ANNOTATIONS;
        String sql = getCachedSql(sqlKeyEnum, annotationClassNames.length);
        if (sql == null) {
            sql = "select distinct " + JACGSqlUtil.joinColumns(DC.MA_FULL_METHOD, DC.MA_METHOD_HASH) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            sql = cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
        }

        List<Map<String, Object>> list = dbOperator.queryList(sql, annotationClassNames);
        if (list == null) {
            return null;
        }

        List<MethodAndHash> methodAndHashList = new ArrayList<>(list.size());
        for (Map<String, Object> map : list) {
            methodAndHashList.add(new MethodAndHash((String) map.get(DC.MA_FULL_METHOD), (String) map.get(DC.MA_METHOD_HASH)));
        }
        return methodAndHashList;
    }

    /**
     * 从方法注解表，查询带有指定注解的，且在指定类中的完整方法及方法HASH
     *
     * @param className            完整类名
     * @param annotationClassNames 注解类名
     * @return
     */
    public List<MethodAndHash> getMethodsAndHashWithAnnotationsOfClass(String className, String... annotationClassNames) {
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

        List<Map<String, Object>> list = dbOperator.queryList(sql, argList.toArray());
        if (list == null) {
            return null;
        }

        List<MethodAndHash> methodAndHashList = new ArrayList<>(list.size());
        for (Map<String, Object> map : list) {
            methodAndHashList.add(new MethodAndHash((String) map.get(DC.MA_FULL_METHOD), (String) map.get(DC.MA_METHOD_HASH)));
        }
        return methodAndHashList;
    }

    /**
     * 从方法注解表，查询带有指定注解的完整方法
     *
     * @param annotationClassNames 注解类名数组
     * @return
     */
    public List<String> getMethodsWithAnnotations(String... annotationClassNames) {
        if (ArrayUtils.isEmpty(annotationClassNames)) {
            return null;
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MA_QUERY_FULL_METHOD_WITH_ANNOTATIONS;
        String sql = getCachedSql(sqlKeyEnum, annotationClassNames.length);
        if (sql == null) {
            sql = "select " + DC.MA_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName() +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            sql = cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, annotationClassNames);
        return JACGSqlUtil.getListString(list);
    }

    /**
     * 从类注解表，查询带有指定注解的完整类名，结果会去重
     *
     * @param querySimpleClassName true: 查询唯一类名 false: 查询完整类名
     * @param annotationClassNames 注解类名
     * @return
     */
    public List<String> getClassesWithAnnotations(boolean querySimpleClassName, String... annotationClassNames) {
        if (ArrayUtils.isEmpty(annotationClassNames)) {
            return null;
        }

        SqlKeyEnum sqlKeyEnum = querySimpleClassName ? SqlKeyEnum.CA_QUERY_SIMPLE_CLASS_NAME_WITH_ANNOTATION : SqlKeyEnum.CA_QUERY_CLASS_NAME_WITH_ANNOTATION;
        String sql = getCachedSql(sqlKeyEnum, annotationClassNames.length);
        if (sql == null) {
            sql = "select distinct " + (querySimpleClassName ? DC.CA_SIMPLE_CLASS_NAME : DC.CA_CLASS_NAME) +
                    " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName() +
                    " where " + DC.CA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            sql = cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, annotationClassNames);
        return JACGSqlUtil.getListString(list);
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

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{methodHash});
        if (JavaCGUtil.isCollectionEmpty(list)) {
            logger.error("根据调用者完整方法HASH+长度未找到完整方法 {}", methodHash);
            return null;
        }

        return (String) list.get(0);
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

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{methodHash});
        if (JavaCGUtil.isCollectionEmpty(list)) {
            logger.error("根据被调用者完整方法HASH+长度未找到完整方法 {}", methodHash);
            return null;
        }

        return (String) list.get(0);
    }

    /**
     * 根据被调用完整方法HASH+长度，与被调用对象类型，查询对应的方法调用信息
     *
     * @param calleeMethodHash
     * @param calleeObjType
     * @return
     */
    public List<MethodCallPair> getMethodCallByCalleeHashObjType(String calleeMethodHash, String calleeObjType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_METHOD_CALL_BY_CALLEE_HASH_OBJ_TYPE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALLER_FULL_METHOD, DC.MC_CALLER_LINE_NUMBER, DC.MC_CALLEE_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " and " + DC.MC_CALLEE_OBJ_TYPE + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{calleeMethodHash, calleeObjType});
        return genMethodCallPairList(list);
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
                    " where " + DC.CN_CLASS_NAME + " = " + DC.CN_SIMPLE_CLASS_NAME
                    + " and " + DC.CN_SIMPLE_CLASS_NAME + " like '%.%'";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, null);
        if (list == null) {
            duplicateSimpleClassNameSet = null;
            return false;
        }
        if (list.isEmpty()) {
            return true;
        }

        for (Object object : list) {
            String duplicateSimpleClassName = JACGClassMethodUtil.getSimpleClassNameFromFull((String) object);
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
        Set<String> foundDuplicateSimpleClassNameSet = new HashSet<>();

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_QUERY_DUPLICATE_CLASS_BEFORE_UPDATE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.CN_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " group by " + DC.CN_SIMPLE_CLASS_NAME +
                    " having count(" + DC.CN_SIMPLE_CLASS_NAME + ") > 1";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, null);
        if (list == null) {
            return null;
        }

        for (Object object : list) {
            foundDuplicateSimpleClassNameSet.add((String) object);
        }
        return foundDuplicateSimpleClassNameSet;
    }

    /**
     * 将类名表中的同名类更新为使用完整类名，并记录同名类
     *
     * @return
     */
    public boolean updateAllSimpleName2Full() {
        Set<String> foundDuplicateSimpleClassNameSet = findDuplicateClassBeforeUpdate();
        if (foundDuplicateSimpleClassNameSet == null) {
            return false;
        }

        if (foundDuplicateSimpleClassNameSet.isEmpty()) {
            logger.info("不存在类名相同但包名不同的类");
            return true;
        }

        logger.info("找到类名相同但包名不同的类 {}", StringUtils.join(foundDuplicateSimpleClassNameSet, " "));
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.CN_UPDATE_SIMPLE_2_FULL;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "update " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName() +
                    " set " + DC.CN_SIMPLE_CLASS_NAME + " = " + DC.CN_CLASS_NAME +
                    " where " + DC.CN_SIMPLE_CLASS_NAME + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        for (String duplicateClassName : foundDuplicateSimpleClassNameSet) {
            // 将class_name_表的simple_name更新为full_name
            if (dbOperator.update(sql, new Object[]{duplicateClassName}) == null) {
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

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{});
        if (JavaCGUtil.isCollectionEmpty(list) || list.get(0) == null) {
            return JACGConstants.MAX_METHOD_CALL_ID_ILLEGAL;
        }

        return (int) list.get(0);
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

        List<Object> list = dbOperator.queryListOneColumn(sql, argList.toArray());
        return JACGSqlUtil.getListString(list);
    }

    /**
     * 根据被调用方法HASH，判断方法调用表中是否存在常规的方法调用（排除ITF、SCC、CCS）
     *
     * @param calleeMethodHash 被调用方法HASH
     * @return null: 出现异常 FALSE: 不存在 TRUE: 存在
     */
    public Boolean checkExistsNormalMethodCallByCalleeMethodHash(String calleeMethodHash) {
        if (calleeMethodHash == null) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CHECK_NORMAL_MC_BY_EE_HASH;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLEE_METHOD_HASH +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ? and " +
                    DC.MC_CALL_TYPE + " not in (?, ?, ?)";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{calleeMethodHash,
                JavaCGCallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS.getType(),
                JavaCGCallTypeEnum.CTE_SUPER_CALL_CHILD.getType(),
                JavaCGCallTypeEnum.CTE_CHILD_CALL_SUPER.getType()
        });
        if (list == null) {
            return null;
        }

        return !list.isEmpty();
    }

    /**
     * 查询方法调用中的调用方与被调用方，使用被调用方完整类名与方法
     *
     * @param calleeClassName  被调用方完整类名
     * @param calleeMethodName 被调用方方法
     * @return
     */
    public List<MethodCallPair> getMethodCallByCalleeFullClassMethod(String calleeClassName, String calleeMethodName) {
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

        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{calleeSimpleClassName, calleeMethodName});
        return genMethodCallPairList(list);
    }

    /**
     * 生成方法调用列表
     *
     * @param list
     * @return
     */
    private List<MethodCallPair> genMethodCallPairList(List<Map<String, Object>> list) {
        if (list == null) {
            return null;
        }
        List<MethodCallPair> methodCallPairList = new ArrayList<>(list.size());
        for (Map<String, Object> map : list) {
            String callerFullMethod = (String) map.get(DC.MC_CALLER_FULL_METHOD);
            int callerLineNumber = (int) map.get(DC.MC_CALLER_LINE_NUMBER);
            String calleeFullMethod = (String) map.get(DC.MC_CALLEE_FULL_METHOD);
            MethodCallPair methodCallPair = new MethodCallPair(callerFullMethod, callerLineNumber, calleeFullMethod);
            methodCallPairList.add(methodCallPair);
        }
        return methodCallPairList;
    }

    /**
     * 查询Lambda表达式中被调用方法信息
     *
     * @param methodCallId
     * @return
     */
    public ClassAndMethodName getLambdaCalleeInfo(int methodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.LMI_QUERY_CALLEE_INFO;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.LMI_LAMBDA_CALLEE_CLASS_NAME, DC.LMI_LAMBDA_CALLEE_METHOD_NAME) +
                    " from " + DbTableInfoEnum.DTIE_LAMBDA_METHOD_INFO.getTableName() +
                    " where " + DC.LMI_CALL_ID + " = ?";
            sql = cacheSql(sqlKeyEnum, sql);
        }

        Map<String, Object> map = dbOperator.queryOneRow(sql, new Object[]{methodCallId});
        if (map == null) {
            return null;
        }

        String className = (String) map.get(DC.LMI_LAMBDA_CALLEE_CLASS_NAME);
        String methodName = (String) map.get(DC.LMI_LAMBDA_CALLEE_METHOD_NAME);
        return new ClassAndMethodName(className, methodName);
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

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{methodHash});
        if (list == null) {
            return null;
        }
        return (Integer) list.get(0);
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

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{simpleClassName, methodName});
        return JACGSqlUtil.getListString(list);
    }

    /**
     * 根据被调用类名与方法名查询调用方信息
     *
     * @param calleeClassName
     * @param calleeMethodName
     * @return
     */
    public List<CallGraphCallerInfo> getCallerInfoByCallee(String calleeClassName, String calleeMethodName) {
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

        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{calleeSimpleClassName, calleeMethodName});
        if (JavaCGUtil.isCollectionEmpty(list)) {
            return null;
        }
        List<CallGraphCallerInfo> callGraphCallerInfoList = new ArrayList<>(list.size());
        for (Map<String, Object> map : list) {
            Integer callId = (Integer) map.get(DC.MC_CALL_ID);
            String callerFullMethod = (String) map.get(DC.MC_CALLER_FULL_METHOD);
            Integer callFlags = (Integer) map.get(DC.MC_CALL_FLAGS);
            CallGraphCallerInfo callGraphCallerInfo = new CallGraphCallerInfo(callId, callerFullMethod, callFlags);
            callGraphCallerInfoList.add(callGraphCallerInfo);
        }
        return callGraphCallerInfoList;
    }

    public String getAppName() {
        return appName;
    }

    public DbOperator getDbOperator() {
        return dbOperator;
    }
}
