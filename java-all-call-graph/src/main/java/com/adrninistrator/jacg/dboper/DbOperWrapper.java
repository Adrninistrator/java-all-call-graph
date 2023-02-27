package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.dto.method.MethodAndHash;
import com.adrninistrator.jacg.dto.method_call.MethodCallPair;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.enums.CallTypeEnum;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
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

    private void cacheSql(String sqlKey, String sql, String sqlKey4Print, int num) {
        // 根据sql语句的key与参数数量，生成最终的key
        String finalSqlKey = genSqlKey(sqlKey, num);
        if (sqlCacheMap.putIfAbsent(finalSqlKey, sql) == null) {
            // 假如有指定用于在日志中打印的key，则在日志中打印出来
            logger.info("[{}] cache sql: [{} {}] [{}]", objSeq, finalSqlKey, sqlKey4Print, sql);
        }
    }

    /**
     * 缓存sql，参数数量可变
     *
     * @param sqlKeyEnum
     * @param sql
     * @param num
     */
    public void cacheSql(SqlKeyEnum sqlKeyEnum, String sql, int num) {
        cacheSql(String.valueOf(sqlKeyEnum.ordinal()), sql, sqlKeyEnum.name(), num);
    }

    private void cacheSql(String sqlKey, String sql, String key4Print) {
        cacheSql(sqlKey, sql, key4Print, 0);
    }

    /**
     * 缓存sql，参数数量固定
     *
     * @param sqlKeyEnum
     * @param sql
     */
    public void cacheSql(SqlKeyEnum sqlKeyEnum, String sql) {
        cacheSql(String.valueOf(sqlKeyEnum.ordinal()), sql, sqlKeyEnum.name());
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
            cacheSql(key, sql, key4Print);
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
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName(appName) +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
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
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName(appName) +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length) +
                    " and " + DC.MA_FULL_METHOD + " like concat(?, '%')";
            cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
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
                    " from " + DbTableInfoEnum.DTIE_METHOD_ANNOTATION.getTableName(appName) +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
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
                    " from " + DbTableInfoEnum.DTIE_CLASS_ANNOTATION.getTableName(appName) +
                    " where " + DC.CA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            cacheSql(sqlKeyEnum, sql, annotationClassNames.length);
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
    public String getCallerFullMethodFromHash(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLER_FULL_METHOD_BY_HASH;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(appName) +
                    " where " + DC.MC_CALLER_METHOD_HASH + " = ?" +
                    " limit 1";
            cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{methodHash});
        if (JACGUtil.isCollectionEmpty(list)) {
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
    public String getCalleeFullMethodFromHash(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLEE_FULL_METHOD_BY_HASH;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLEE_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(appName) +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " limit 1";
            cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{methodHash});
        if (JACGUtil.isCollectionEmpty(list)) {
            logger.error("根据被调用者完整方法HASH+长度未找到完整方法 {}", methodHash);
            return null;
        }

        return (String) list.get(0);
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
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName(appName) +
                    " where " + DC.CN_CLASS_NAME + " = " + DC.CN_SIMPLE_CLASS_NAME
                    + " and " + DC.CN_SIMPLE_CLASS_NAME + " like '%.%'";
            cacheSql(sqlKeyEnum, sql);
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
                    " from " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName(appName) +
                    " group by " + DC.CN_SIMPLE_CLASS_NAME +
                    " having count(" + DC.CN_SIMPLE_CLASS_NAME + ") > 1";
            cacheSql(sqlKeyEnum, sql);
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
            sql = "update " + DbTableInfoEnum.DTIE_CLASS_NAME.getTableName(appName) +
                    " set " + DC.CN_SIMPLE_CLASS_NAME + " = " + DC.CN_CLASS_NAME +
                    " where " + DC.CN_SIMPLE_CLASS_NAME + " = ?";
            cacheSql(sqlKeyEnum, sql);
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
            sql = "select max(" + DC.MC_CALL_ID + ") from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(appName);
            cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{});
        if (JACGUtil.isCollectionEmpty(list) || list.get(0) == null) {
            return JACGConstants.MAX_METHOD_CALL_ID_ILLEGAL;
        }

        return (int) list.get(0);
    }

    /**
     * 人工向方法调用表写入数据
     * 在原有向数据库写入数据操作完成之后执行
     * 使用自定义框架导致方法调用关系在字节码中缺失时使用，例如使用XML、注解等方式的情况
     *
     * @param callerFullMethod 调用者完整方法
     * @param calleeFullMethod 被调用者完整方法
     * @return
     */
    public boolean manualAddMethodCall(String callerFullMethod, String calleeFullMethod) {
        if (StringUtils.isAnyBlank(callerFullMethod, calleeFullMethod)) {
            logger.error("调用方法与被调用方法不允许为空 {} {}", callerFullMethod, calleeFullMethod);
            return false;
        }

        if (StringUtils.equals(callerFullMethod, calleeFullMethod)) {
            logger.error("调用方法与被调用方法不允许相同 {}", callerFullMethod);
            return false;
        }

        // 查询数据库方法调用表最大序号
        int maxCallId = getMaxMethodCallId();
        if (maxCallId == JACGConstants.MAX_METHOD_CALL_ID_ILLEGAL) {
            return false;
        }

        String callerClassName = JACGClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        String calleeClassName = JACGClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        logger.info("[{}] 人工向数据库方法调用表加入数据 {} {} {}", objSeq, maxCallId + 1, callerFullMethod, calleeFullMethod);
        // 人工向方法调用表写入数据，行号使用0，jar包序号使用0
        WriteDbData4MethodCall writeDbData4MethodCall = WriteDbData4MethodCall.genInstance(CallTypeEnum.CTE_MANUAL_ADDED.getType(),
                getSimpleClassName(callerClassName),
                callerFullMethod,
                getSimpleClassName(calleeClassName),
                calleeFullMethod,
                ++maxCallId,
                JavaCGConstants.DEFAULT_LINE_NUMBER,
                String.valueOf(0)
        );
        Object[] arguments = JACGUtil.genMethodCallObjectArray(writeDbData4MethodCall);

        String sql = genAndCacheInsertSql(DbTableInfoEnum.DTIE_METHOD_CALL.getSqlKey(),
                DbTableInfoEnum.DTIE_METHOD_CALL.getSqlKey4Print(),
                DbInsertMode.DIME_INSERT,
                DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(appName),
                DbTableInfoEnum.DTIE_METHOD_CALL.getColumns());
        return dbOperator.insert(sql, arguments);
    }

    /**
     * 查询调用方法时包含指定方法调用自定义数据的调用者完整方法
     *
     * @param dataTypeList    方法调用自定义数据类型，可指定多个，关系为或（in xxx）
     * @param dataKeywordList 方法调用自定义数据关键字，可指定多个，关系为且（like xxx and like xxx）
     * @return
     */
    public List<String> getCallerFullMethodWithExtendedData(List<String> dataTypeList, List<String> dataKeywordList) {
        if (dataKeywordList == null || dataTypeList == null) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.ED_QUERY_METHOD_BY_EXTENDED_DATA;
        String sql = getCachedSql(sqlKeyEnum, dataKeywordList.size());
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLER_FULL_METHOD + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(appName) +
                    " where " + DC.MC_CALL_ID + " in " +
                    " (" +
                    " select " + DC.ED_CALL_ID +
                    " from " + DbTableInfoEnum.DTIE_EXTENDED_DATA.getTableName(appName) +
                    " where " + DC.ED_DATA_TYPE + " in " + JACGSqlUtil.genQuestionString(dataTypeList.size()) +
                    StringUtils.repeat(" and " + DC.ED_DATA_VALUE + " like concat('%', ?, '%')", dataKeywordList.size()) +
                    ")";
            cacheSql(sqlKeyEnum, sql);
        }

        List<String> argList = new ArrayList<>(dataTypeList.size() + dataKeywordList.size());
        argList.addAll(dataTypeList);
        argList.addAll(dataKeywordList);

        List<Object> list = dbOperator.queryListOneColumn(sql, argList.toArray());
        return JACGSqlUtil.getListString(list);
    }

    /**
     * 从方法调用表根据被调用者完整方法查询调用者完整方法
     * 完整匹配字符串
     *
     * @param calleeFullMethod 被调用者完整方法
     * @return
     */
    public List<String> getCallerFullMethodByCalleeFullMethod(String calleeFullMethod) {
        if (calleeFullMethod == null) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_ERFM_BY_EEFM;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(appName)
                    + " where " + DC.MC_CALLEE_FULL_METHOD + " = ?";
            cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{calleeFullMethod});
        return JACGSqlUtil.getListString(list);
    }

    /**
     * 从方法调用表根据被调用者完整方法查询调用者完整方法
     * 使用前缀like匹配
     *
     * @param calleeFullMethod 被调用者完整方法
     * @return
     */
    public List<String> getCallerFullMethodByCalleeFullMethodLikePrefix(String calleeFullMethod) {
        if (calleeFullMethod == null) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_ERFM_BY_EEFM_LIKE_PREFIX;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(appName)
                    + " where " + DC.MC_CALLEE_FULL_METHOD + " like concat (?, '%')";
            cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{calleeFullMethod});
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
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(appName)
                    + " where " + DC.MC_CALLEE_METHOD_HASH + " = ? and " +
                    DC.MC_CALL_TYPE + " not in (?, ?, ?)";
            cacheSql(sqlKeyEnum, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{calleeMethodHash,
                CallTypeEnum.CTE_INTERFACE_CALL_IMPL_CLASS.getType(),
                CallTypeEnum.CTE_SUPER_CALL_CHILD.getType(),
                CallTypeEnum.CTE_CHILD_CALL_SUPER.getType()
        });
        if (list == null) {
            return null;
        }

        return !list.isEmpty();
    }

    /**
     * 查询方法调用中的调用方与被调用方，使用被调用方完整类名与方法
     *
     * @param calleeFullClassName 被调用方唯一类名
     * @param calleeMethodName    被调用方方法
     * @return
     */
    public List<MethodCallPair> getMethodCallByCalleeFullClassMethod(String calleeFullClassName, String calleeMethodName) {
        if (calleeFullClassName == null || calleeMethodName == null) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        String calleeSimpleClassName = getSimpleClassName(calleeFullClassName);
        return getMethodCallByCalleeSimpleClassMethod(calleeSimpleClassName, calleeMethodName);
    }

    /**
     * 查询方法调用中的调用方与被调用方，使用被调用方唯一类名与方法
     *
     * @param calleeSimpleClassName 被调用方唯一类名
     * @param calleeMethodName      被调用方方法
     * @return
     */
    public List<MethodCallPair> getMethodCallByCalleeSimpleClassMethod(String calleeSimpleClassName, String calleeMethodName) {
        if (calleeSimpleClassName == null || calleeMethodName == null) {
            throw new JavaCGRuntimeException("参数不允许为空");
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_MC_PAIR_BY_CALLEE;
        String sql = getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALLER_FULL_METHOD, DC.MC_CALLER_LINE_NUMBER, DC.MC_CALLEE_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName(appName)
                    + " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ? and " +
                    DC.MC_CALLEE_METHOD_NAME + " = ?";
            cacheSql(sqlKeyEnum, sql);
        }

        List<Map<String, Object>> list = dbOperator.queryList(sql, new Object[]{calleeSimpleClassName, calleeMethodName});
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

    public String getAppName() {
        return appName;
    }
}
