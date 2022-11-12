package com.adrninistrator.jacg.dboper;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.dto.entity.MethodCallEntity;
import com.adrninistrator.jacg.dto.method.MethodAndHash;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.enums.CallTypeEnum;
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
    private Set<String> duplicateClassNameSet = null;

    private final DbOperator dbOperator;

    private final String appName;

    private final String objSeq;

    public DbOperWrapper(DbOperator dbOperator, String appName) {
        this.dbOperator = dbOperator;
        this.appName = appName;

        objSeq = String.valueOf(ATOMIC_INTEGER.incrementAndGet());
        logger.info("objSeq [{}]", objSeq);
    }

    private String genSqlKey(String sqlKey, int num) {
        if (num == 0) {
            return sqlKey;
        }
        return sqlKey + JACGConstants.FLAG_AT + num;
    }

    public String getCachedSql(String sqlKey, int num) {
        return sqlCacheMap.get(genSqlKey(sqlKey, num));
    }

    public String getCachedSql(String sqlKey) {
        return getCachedSql(sqlKey, 0);
    }

    public void cacheSql(String sqlKey, String sql, int num) {
        String finalSqlKey = genSqlKey(sqlKey, num);
        if (sqlCacheMap.putIfAbsent(finalSqlKey, sql) == null) {
            logger.info("[{}] cache sql: [{}] [{}]", objSeq, finalSqlKey, sql);
        }
    }

    public void cacheSql(String sqlKey, String sql) {
        cacheSql(sqlKey, sql, 0);
    }

    public String genAndCacheInsertSql(String key, DbInsertMode dbInsertMode, String tableName, String[] columns) {
        String sql = getCachedSql(key);
        if (sql == null) {
            sql = dbInsertMode.getMode();
            sql = sql + tableName + appName + JACGSqlUtil.genColumnString(columns) + " values " + JACGSqlUtil.genQuestionString(columns.length);
            cacheSql(key, sql);
        }
        return sql;
    }

    /**
     * 从方法注解表，查询带有指定注解的完整方法及方法HASH
     *
     * @param annotationClassNames 注解类名
     * @return
     */
    public List<MethodAndHash> getMethodsAndHashWithAnnotations(String[] annotationClassNames) {
        if (ArrayUtils.isEmpty(annotationClassNames)) {
            return null;
        }

        String sqlKey = JACGConstants.SQL_KEY_MA_QUERY_FMAH_WITH_ANNOTATIONS;
        String sql = getCachedSql(sqlKey, annotationClassNames.length);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MA_FULL_METHOD, DC.MA_METHOD_HASH) +
                    " from " + JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION + appName +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            cacheSql(sqlKey, sql, annotationClassNames.length);
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
     * @param fullClassName        完整类名
     * @param annotationClassNames 注解类名
     * @return
     */
    public List<MethodAndHash> getMethodsAndHashWithAnnotationsOfClass(String fullClassName, String[] annotationClassNames) {
        if (fullClassName == null || ArrayUtils.isEmpty(annotationClassNames)) {
            return null;
        }

        String sqlKey = JACGConstants.SQL_KEY_MA_QUERY_FMAH_WITH_ANNOTATIONS_OF_CLASS;
        String sql = getCachedSql(sqlKey, annotationClassNames.length);
        if (sql == null) {
            // 指定完整方法需要以[完整类名]:开关，只查询指定类中的方法
            sql = "select " + JACGSqlUtil.joinColumns(DC.MA_FULL_METHOD, DC.MA_METHOD_HASH) +
                    " from " + JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION + appName +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length) +
                    " and " + DC.MA_FULL_METHOD + " like concat(?, ?, '%')";
            cacheSql(sqlKey, sql, annotationClassNames.length);
        }

        List<String> argList = new ArrayList<>(annotationClassNames.length + 2);
        argList.addAll(Arrays.asList(annotationClassNames));
        argList.add(fullClassName);
        argList.add(JACGConstants.FLAG_COLON);

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
    public List<String> getMethodsWithAnnotations(String[] annotationClassNames) {
        if (ArrayUtils.isEmpty(annotationClassNames)) {
            return null;
        }

        String sqlKey = JACGConstants.SQL_KEY_MA_QUERY_FULL_METHOD_WITH_ANNOTATIONS;
        String sql = getCachedSql(sqlKey, annotationClassNames.length);
        if (sql == null) {
            sql = "select " + DC.MA_FULL_METHOD +
                    " from " + JACGConstants.TABLE_PREFIX_METHOD_ANNOTATION + appName +
                    " where " + DC.MA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            cacheSql(sqlKey, sql, annotationClassNames.length);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, annotationClassNames);
        return JACGSqlUtil.getListString(list);
    }

    /**
     * 从类注解表，查询带有指定注解的完整类名
     *
     * @param annotationClassNames 注解类名数组
     * @return
     */
    public List<String> getClassesWithAnnotations(String[] annotationClassNames) {
        if (ArrayUtils.isEmpty(annotationClassNames)) {
            return null;
        }

        String sqlKey = JACGConstants.SQL_KEY_CA_QUERY_FULL_CLASS_NAME_WITH_ANNOTATION;
        String sql = getCachedSql(sqlKey, annotationClassNames.length);
        if (sql == null) {
            sql = "select " + DC.CA_FULL_CLASS_NAME +
                    " from " + JACGConstants.TABLE_PREFIX_CLASS_ANNOTATION + appName +
                    " where " + DC.CA_ANNOTATION_NAME + " in " + JACGSqlUtil.genQuestionString(annotationClassNames.length);
            cacheSql(sqlKey, sql, annotationClassNames.length);
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
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_CALLER_FULL_METHOD_BY_HASH;
        String sql = getCachedSql(sqlKey);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_METHOD + " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + appName +
                    " where " + DC.MC_CALLER_METHOD_HASH + " = ?" +
                    " limit 1";
            cacheSql(sqlKey, sql);
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
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_CALLEE_FULL_METHOD_BY_HASH;
        String sql = getCachedSql(sqlKey);
        if (sql == null) {
            sql = "select " + DC.MC_CALLEE_FULL_METHOD + " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + appName +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " limit 1";
            cacheSql(sqlKey, sql);
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
        duplicateClassNameSet = new HashSet<>();

        String sqlKey = JACGConstants.SQL_KEY_CN_QUERY_DUPLICATE_CLASS;
        String sql = getCachedSql(sqlKey);
        if (sql == null) {
            sql = "select " + DC.CN_SIMPLE_NAME +
                    " from " + JACGConstants.TABLE_PREFIX_CLASS_NAME + appName +
                    " group by " + DC.CN_SIMPLE_NAME +
                    " having count(" + DC.CN_SIMPLE_NAME + ") > 1";
            cacheSql(sqlKey, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, null);
        if (list == null) {
            return false;
        }

        for (Object object : list) {
            String duplicateClassName = (String) object;
            duplicateClassNameSet.add(duplicateClassName);
        }
        return true;
    }

    /**
     * 将类名表中的同名类更新为使用完整类名
     *
     * @return
     */
    public boolean updateAllSimpleName2Full() {
        if (duplicateClassNameSet == null) {
            logger.error("还未查询同名类");
            return false;
        }

        String sqlKey = JACGConstants.SQL_KEY_CN_UPDATE_SIMPLE_2_FULL;
        String sql = getCachedSql(sqlKey);
        if (sql == null) {
            sql = "update " + JACGConstants.TABLE_PREFIX_CLASS_NAME + appName +
                    " set " + DC.CN_SIMPLE_NAME + " = " + DC.CN_FULL_NAME +
                    " where " + DC.CN_SIMPLE_NAME + " = ?";
            cacheSql(sqlKey, sql);
        }

        for (String duplicateClassName : duplicateClassNameSet) {
            // 将class_name_表的simple_name更新为full_name
            if (dbOperator.update(sql, new Object[]{duplicateClassName}) == null) {
                return false;
            }
        }

        return true;
    }

    /**
     * 根据完整类名获取对应的类名
     * 若当前简单类名存在1个以上，则返回完整类名
     * 若当前简单类名只有1个，则返回简单类名
     *
     * @param fullClassName 完整类名信息
     * @return 完整类名或简单类名
     */
    public String getFullOrSimpleClassName(String fullClassName) {
        String simpleClassName = JACGUtil.getSimpleClassNameFromFull(fullClassName);
        if (duplicateClassNameSet.contains(simpleClassName)) {
            return fullClassName;
        }
        return simpleClassName;
    }

    /**
     * 生成方法调用数据
     *
     * @param callType
     * @param callerFullMethod
     * @param calleeFullMethod
     * @param callId
     * @param callerLineNum
     * @param callerJarNum
     * @return
     */
    public MethodCallEntity genMethodCallEntity(String callType, String callerFullMethod, String calleeFullMethod, int callId, int callerLineNum, String callerJarNum) {
        logger.debug("方法调用数据\r\n[{}]\r\n[{}]\r\n[{}]\r\n[{}]", callType, callerFullMethod, calleeFullMethod, callerLineNum);

        String callerMethodHash = JACGUtil.genHashWithLen(callerFullMethod);
        String callerMethodName = JACGUtil.getMethodNameFromFull(callerFullMethod);
        String callerFullClassName = JACGUtil.getFullClassNameFromMethod(callerFullMethod);
        String callerSimpleClassName = getFullOrSimpleClassName(callerFullClassName);

        MethodCallEntity methodCallEntity = new MethodCallEntity();
        methodCallEntity.setId(callId);
        methodCallEntity.setCallType(callType);
        methodCallEntity.setEnabled(JACGConstants.ENABLED);
        methodCallEntity.setCallerJarNum(callerJarNum);
        methodCallEntity.setCallerMethodHash(callerMethodHash);
        methodCallEntity.setCallerFullMethod(callerFullMethod);
        methodCallEntity.setCallerMethodName(callerMethodName);
        methodCallEntity.setCallerFullClassName(callerFullClassName);
        methodCallEntity.setCallerClassName(callerSimpleClassName);
        methodCallEntity.setCallerLineNum(callerLineNum);

        // 设置调用数据中的被调用方法信息
        setCallee4MethodCallEntity(methodCallEntity, calleeFullMethod);

        return methodCallEntity;
    }

    /**
     * 设置调用数据中的被调用方法信息
     *
     * @param methodCallEntity
     * @param calleeFullMethod
     */
    private void setCallee4MethodCallEntity(MethodCallEntity methodCallEntity, String calleeFullMethod) {
        String calleeMethodHash = JACGUtil.genHashWithLen(calleeFullMethod);
        String calleeMethodName = JACGUtil.getMethodNameFromFull(calleeFullMethod);
        String calleeFullClassName = JACGUtil.getFullClassNameFromMethod(calleeFullMethod);
        String calleeSimpleClassName = getFullOrSimpleClassName(calleeFullClassName);
        methodCallEntity.setCalleeMethodHash(calleeMethodHash);
        methodCallEntity.setCalleeFullMethod(calleeFullMethod);
        methodCallEntity.setCalleeMethodName(calleeMethodName);
        methodCallEntity.setCalleeFullClassName(calleeFullClassName);
        methodCallEntity.setCalleeClassName(calleeSimpleClassName);
    }

    /**
     * 查询数据库方法调用表最大序号
     *
     * @return
     */
    private int getMaxMethodCallId() {
        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_MAX_CALL_ID;
        String sql = getCachedSql(sqlKey);
        if (sql == null) {
            sql = "select max(" + DC.MC_CALL_ID + ") from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + appName;
            cacheSql(sqlKey, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{});
        if (list == null) {
            return JACGConstants.MAX_METHOD_CALL_ID_ILLEGAL;
        }

        return (int) list.get(0);
    }

    /**
     * 人工向方法调用表写入数据
     * 在原有向数据库写入数据操作完成之后执行
     *
     * @param callerFullMethod 调用者完整方法
     * @param calleeFullMethod 被调用者完整方法
     * @return
     */
    public boolean manualAddMethodCall(String callerFullMethod, String calleeFullMethod) {
        if (StringUtils.isAnyBlank(callerFullMethod, calleeFullMethod)) {
            logger.error("传入参数不允许为空 {} {}", callerFullMethod, calleeFullMethod);
            return false;
        }

        if (StringUtils.equals(callerFullMethod, calleeFullMethod)) {
            logger.error("传入参数不允许相同 {}", callerFullMethod);
            return false;
        }

        // 查找类名相同但包名不同的类
        if (duplicateClassNameSet == null && !findDuplicateClass()) {
            return false;
        }

        // 查询数据库方法调用表最大序号
        int maxCallId = getMaxMethodCallId();
        if (maxCallId == JACGConstants.MAX_METHOD_CALL_ID_ILLEGAL) {
            return false;
        }

        logger.info("[{}] 人工向数据库方法调用表加入数据 {} {} {}", objSeq, maxCallId + 1, callerFullMethod, calleeFullMethod);
        // 人工向方法调用表写入数据，行号使用0，jar包序号使用0
        MethodCallEntity methodCallEntity = genMethodCallEntity(CallTypeEnum.CTE_MA.getType(), callerFullMethod, calleeFullMethod, maxCallId + 1, 0,
                String.valueOf(0));

        List<Object[]> tmpMethodCallList = genMethodCallList(Collections.singletonList(methodCallEntity));
        if (!writeMethodCall2Db(tmpMethodCallList)) {
            logger.error("人工向数据库方法调用表加入数据失败 {} {}", callerFullMethod, calleeFullMethod);
            return false;
        }
        return true;
    }

    /**
     * 生成用于写入数据库方法调用表的列表
     *
     * @param methodCallList
     * @return
     */
    public List<Object[]> genMethodCallList(List<MethodCallEntity> methodCallList) {
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
                    methodCallEntity.getCallerClassName(),
                    methodCallEntity.getCallerLineNum(),
                    methodCallEntity.getCalleeMethodHash(),
                    methodCallEntity.getCalleeFullMethod(),
                    methodCallEntity.getCalleeMethodName(),
                    methodCallEntity.getCalleeFullClassName(),
                    methodCallEntity.getCalleeClassName()
            };
            tmpMethodCallList.add(object);
        }
        return tmpMethodCallList;
    }

    /**
     * 写入数据库方法调用表
     *
     * @param methodCallList
     * @return
     */
    public boolean writeMethodCall2Db(List<Object[]> methodCallList) {
        String sqlKey = JACGConstants.SQL_KEY_INSERT_METHOD_CALL;
        String sql = getCachedSql(sqlKey);
        if (sql == null) {
            sql = genAndCacheInsertSql(sqlKey,
                    DbInsertMode.DIME_INSERT,
                    JACGConstants.TABLE_PREFIX_METHOD_CALL,
                    JACGConstants.TABLE_COLUMNS_METHOD_CALL);
        }

        return dbOperator.batchInsert(sql, methodCallList);
    }

    /**
     * 查询调用方法时包含指定自定义数据的调用者完整方法
     *
     * @param dataTypeList    自定义数据类型，可指定多个，关系为或（in xxx）
     * @param dataKeywordList 自定义数据关键字，可指定多个，关系为且（like xxx and like xxx）
     * @return
     */
    public List<String> getCallerFullMethodWithExtendedData(List<String> dataTypeList, List<String> dataKeywordList) {
        if (dataKeywordList == null || dataTypeList == null) {
            return null;
        }

        String sql = "select distinct(" + DC.MC_CALLER_FULL_METHOD + ")" +
                " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + appName +
                " where " + DC.MC_CALL_ID + " in " +
                " (" +
                " select " + DC.ED_CALL_ID +
                " from " + JACGConstants.TABLE_PREFIX_EXTENDED_DATA + appName +
                " where " + DC.ED_DATA_TYPE + " in " + JACGSqlUtil.genQuestionString(dataTypeList.size()) +
                StringUtils.repeat(" and " + DC.ED_DATA_VALUE + " like concat('%', ?, '%')", dataKeywordList.size()) +
                ")";

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
            return null;
        }

        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_ERFM_BY_EEFM;
        String sql = getCachedSql(sqlKey);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_METHOD +
                    " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + appName
                    + " where " + DC.MC_CALLEE_FULL_METHOD + " = ?";
            cacheSql(sqlKey, sql);
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
            return null;
        }

        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_ERFM_BY_EEFM_LIKE_PREFIX;
        String sql = getCachedSql(sqlKey);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_METHOD +
                    " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + appName
                    + " where " + DC.MC_CALLEE_FULL_METHOD + " like concat (?, '%')";
            cacheSql(sqlKey, sql);
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
            return null;
        }

        String sqlKey = JACGConstants.SQL_KEY_MC_QUERY_CHECK_NORMAL_MC_BY_EE_HASH;
        String sql = getCachedSql(sqlKey);
        if (sql == null) {
            sql = "select " + DC.MC_CALLEE_METHOD_HASH +
                    " from " + JACGConstants.TABLE_PREFIX_METHOD_CALL + appName
                    + " where " + DC.MC_CALLEE_METHOD_HASH + " = ? and " +
                    DC.MC_CALL_TYPE + " not in (?, ?, ?)";
            cacheSql(sqlKey, sql);
        }

        List<Object> list = dbOperator.queryListOneColumn(sql, new Object[]{calleeMethodHash,
                CallTypeEnum.CTE_ITF.getType(),
                CallTypeEnum.CTE_SCC.getType(),
                CallTypeEnum.CTE_CCS.getType()
        });
        if (list == null) {
            return null;
        }

        return !list.isEmpty();
    }

    private DbOperWrapper() {
        throw new IllegalStateException("illegal");
    }
}
