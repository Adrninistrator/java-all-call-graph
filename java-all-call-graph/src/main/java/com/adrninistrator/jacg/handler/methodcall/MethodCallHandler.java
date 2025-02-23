package com.adrninistrator.jacg.handler.methodcall;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DbInsertMode;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2CallTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description: 方法调用处理类
 */
public class MethodCallHandler extends BaseHandler {
    private static final Logger logger = LoggerFactory.getLogger(MethodCallHandler.class);

    public MethodCallHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    public MethodCallHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
    }

    /**
     * 启用指定的方法调用
     *
     * @param methodCallId
     * @return
     */
    public boolean enableMethodCall(int methodCallId) {
        return updateMethodCallEnabled(methodCallId, JavaCG2YesNoEnum.YES.getIntValue());
    }

    /**
     * 禁用指定的方法调用
     *
     * @param methodCallId
     * @return
     */
    public boolean disableMethodCall(int methodCallId) {
        return updateMethodCallEnabled(methodCallId, JavaCG2YesNoEnum.NO.getIntValue());
    }

    // 修改方法调用表启用标志
    private boolean updateMethodCallEnabled(int methodCallId, int enabled) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_UPDATE_ENABLED;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "update " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " set " + DC.MC_ENABLED + " = ?" +
                    " where " + DC.MC_CALL_ID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Integer row = dbOperator.update(sql, enabled, methodCallId);
        logger.info("修改方法调用表 {} 启用标志: {} 行数: {}", methodCallId, enabled, row);
        return row != null && row > 0;
    }

    /**
     * 更新方法调用表指定方法调用的标志，增加其他值
     *
     * @param methodCallId
     * @param methodCallFlagsEnum
     * @return
     */
    public boolean updateMethodCallAddFlags(int methodCallId, MethodCallFlagsEnum methodCallFlagsEnum) {
        SqlKeyEnum sqlKeyEnumQuery = SqlKeyEnum.MC_QUERY_FLAG_BY_ID;
        String sqlQuery = dbOperWrapper.getCachedSql(sqlKeyEnumQuery);
        if (sqlQuery == null) {
            sqlQuery = "select " + DC.MC_CALL_FLAGS +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALL_ID + " = ?";
            sqlQuery = dbOperWrapper.cacheSql(sqlKeyEnumQuery, sqlQuery);
        }

        WriteDbData4MethodCall writeDbData4MethodCall = dbOperator.queryObject(sqlQuery, WriteDbData4MethodCall.class, methodCallId);
        if (writeDbData4MethodCall == null) {
            logger.error("未查询到指定方法调用的方法调用标志 {}", methodCallId);
            return false;
        }

        SqlKeyEnum sqlKeyEnumUpdate = SqlKeyEnum.MC_UPDATE_FLAGS;
        String sqlUpdate = dbOperWrapper.getCachedSql(sqlKeyEnumUpdate);
        if (sqlUpdate == null) {
            sqlUpdate = "update " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " set " + DC.MC_CALL_FLAGS + " = ?" +
                    " where " + DC.MC_CALL_ID + " = ?";
            sqlUpdate = dbOperWrapper.cacheSql(sqlKeyEnumUpdate, sqlUpdate);
        }

        int newCallFlags = methodCallFlagsEnum.setFlag(writeDbData4MethodCall.getCallFlags());
        Integer row = dbOperator.update(sqlUpdate, newCallFlags, methodCallId);
        logger.info("修改方法调用表，调用ID为 {} 的方法调用标志: {} 行数: {}", methodCallId, Integer.toBinaryString(newCallFlags), row);
        return row != null && row > 0;
    }

    /**
     * 人工向方法调用表写入数据
     * 在原有向数据库写入数据操作完成之后执行
     * 使用自定义框架导致方法调用关系在字节码中缺失时使用，例如使用XML、注解等方式的情况
     * 当前方法不允许并发调用
     *
     * @param callerFullMethod 调用方完整方法
     * @param calleeFullMethod 被调用方完整方法
     * @return
     */
    public synchronized boolean manualAddMethodCall(String callerFullMethod, String calleeFullMethod) {
        if (StringUtils.isAnyBlank(callerFullMethod, calleeFullMethod)) {
            logger.error("调用方法与被调用方法不允许为空 {} {}", callerFullMethod, calleeFullMethod);
            return false;
        }

        if (StringUtils.equals(callerFullMethod, calleeFullMethod)) {
            logger.error("调用方法与被调用方法不允许相同 {}", callerFullMethod);
            return false;
        }

        // 查询数据库方法调用表最大序号，序号不能重复，因此不能并发调用
        int maxCallId = queryMaxMethodCallId();
        if (maxCallId == JACGConstants.METHOD_CALL_ID_ILLEGAL) {
            return false;
        }

        String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        String calleeClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
        int nextMaxCallId = maxCallId + 1;
        logger.info("人工向数据库方法调用表加入数据 {}\n{}\n{}", nextMaxCallId, callerFullMethod, calleeFullMethod);
        // 人工向方法调用表写入数据，行号使用0，jar包序号使用0
        WriteDbData4MethodCall writeDbData4MethodCall = WriteDbData4MethodCall.genInstance(
                nextMaxCallId,
                true,
                JavaCG2CallTypeEnum.CTE_MANUAL_ADDED.getType(),
                dbOperWrapper.querySimpleClassName(callerClassName),
                callerFullMethod,
                JavaCG2Constants.DEFAULT_LINE_NUMBER,
                "",
                dbOperWrapper.querySimpleClassName(calleeClassName),
                calleeFullMethod,
                0,
                "",
                "",
                "",
                null,
                null,
                "人工添加的方法调用记录"
        );
        String sql = dbOperWrapper.genAndCacheInsertSql(DbTableInfoEnum.DTIE_METHOD_CALL, DbInsertMode.DIME_INSERT);
        return dbOperator.insert(sql, JACGSqlUtil.genMethodCallObjectArray(writeDbData4MethodCall));
    }

    /**
     * 查询指定类的指定代码行号对应的方法调用信息
     *
     * @param callerClassName
     * @param callerLineNumber
     * @return
     */
    public List<WriteDbData4MethodCall> queryMethodCallByCallerClassLineNumber(String callerClassName, int callerLineNumber) {
        SqlKeyEnum sqlKeyEnumQuery = SqlKeyEnum.MC_QUERY_BY_CALLER_CLASS_LINE_NUMBER;
        String sqlQuery = dbOperWrapper.getCachedSql(sqlKeyEnumQuery);
        if (sqlQuery == null) {
            sqlQuery = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLER_LINE_NUMBER + " = ?";
            sqlQuery = dbOperWrapper.cacheSql(sqlKeyEnumQuery, sqlQuery);
        }

        return dbOperator.queryList(sqlQuery, WriteDbData4MethodCall.class, dbOperWrapper.querySimpleClassName(callerClassName), callerLineNumber);
    }

    /**
     * 根据调用方完整方法HASH+长度，从方法调用表获取对应的完整方法
     *
     * @param methodHash 完整方法HASH+长度
     * @return
     */
    public String queryCallerFullMethodByHash(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLER_FULL_METHOD_BY_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLER_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_METHOD_HASH + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String callerFullMethod = dbOperator.queryObjectOneColumn(sql, String.class, methodHash);
        if (callerFullMethod == null) {
            logger.warn("根据调用方完整方法HASH+长度未找到完整方法 {}", methodHash);
        }
        return callerFullMethod;
    }

    /**
     * 根据被调用方完整方法HASH+长度，从方法调用表获取对应的完整方法
     *
     * @param methodHash 完整方法HASH+长度
     * @return
     */
    public String queryCalleeFullMethodByHash(String methodHash) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLEE_FULL_METHOD_BY_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLEE_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        String calleeFullMethod = dbOperator.queryObjectOneColumn(sql, String.class, methodHash);
        if (calleeFullMethod == null) {
            logger.warn("根据被调用方完整方法HASH+长度未找到完整方法 {}", methodHash);
        }
        return calleeFullMethod;
    }


    /**
     * 根据被调用方法类名与方法，查询对应的完整方法
     * 不从method_info表查询，因为被调用方法所在的jar包可能没有解析
     *
     * @param className
     * @param methodName
     * @return
     */
    public List<String> queryCalleeFullMethodByClassMethod(String className, String methodName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLEE_FULL_METHOD_BY_CLASS_METHOD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct " + DC.MC_CALLEE_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLEE_METHOD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(className), methodName);
    }

    /**
     * 根据方法调用序号，从方法调用表获取对应的完整方法
     *
     * @param methodCallId 方法调用序号
     * @return
     */
    public String queryCalleeFullMethodById(int methodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLEE_FULL_METHOD_BY_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLEE_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALL_ID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        String calleeFullMethod = dbOperator.queryObjectOneColumn(sql, String.class, methodCallId);
        if (calleeFullMethod == null) {
            logger.warn("根据方法调用序号未找到完整方法 {}", methodCallId);
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
    public List<WriteDbData4MethodCall> queryMethodCallByCalleeHashObjType(String calleeMethodHash, String calleeObjType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_METHOD_CALL_BY_CALLEE_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALLER_FULL_METHOD, DC.MC_CALLER_LINE_NUMBER, DC.MC_CALLEE_FULL_METHOD) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " and " + DC.MC_CALLEE_OBJ_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, calleeMethodHash, calleeObjType);
    }

    /**
     * 根据被调用完整方法，查询对应的方法调用信息
     *
     * @param calleeFullMethod
     * @return
     */
    public List<WriteDbData4MethodCall> queryMethodCallByCalleeFullMethod(String calleeFullMethod) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_METHOD_CALL_BY_CALLEE_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, JACGUtil.genHashWithLen(calleeFullMethod));
    }

    /**
     * 根据方法调用序号查询对应的方法调用信息
     *
     * @param methodCallId
     * @return
     */
    public WriteDbData4MethodCall queryMethodCallByCallId(int methodCallId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_BY_CALL_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALL_ID + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObject(sql, WriteDbData4MethodCall.class, methodCallId);
    }

    /**
     * 查询数据库方法调用表最大序号
     *
     * @return
     */
    public int queryMaxMethodCallId() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_MAX_CALL_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            // call_id最小值为1，因此默认使用0
            sql = "select ifnull(max(" + DC.MC_CALL_ID + "),?) from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName();
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        Integer maxId = dbOperator.queryObjectOneColumn(sql, Integer.class, JavaCG2Constants.RECORD_ID_MIN_BEFORE);
        return maxId == null ? JACGConstants.METHOD_CALL_ID_ILLEGAL : maxId;
    }

    /**
     * 查询调用方法时包含指定方法调用业务功能数据的调用方完整方法
     *
     * @param dataTypeList    方法调用业务功能数据类型，可指定多个，关系为或（in xxx）
     * @param dataKeywordList 方法调用业务功能数据关键字，可指定多个，关系为且（like xxx and like xxx）
     * @return
     */
    public List<String> queryCallerFullMethodWithBusinessData(List<String> dataTypeList, List<String> dataKeywordList) {
        if (dataKeywordList == null || dataTypeList == null) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }

        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.BD_QUERY_METHOD_BY_BUSINESS_DATA;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum, dataKeywordList.size());
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
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql, dataKeywordList.size());
        }

        List<String> argList = new ArrayList<>(dataTypeList.size() + dataKeywordList.size());
        argList.addAll(dataTypeList);
        argList.addAll(dataKeywordList);

        return dbOperator.queryListOneColumn(sql, String.class, argList.toArray());
    }

    /**
     * 根据被调用方法HASH，判断方法调用表中是否存在常规的方法调用（排除实现、继承相关的方法调用）
     *
     * @param calleeMethodHash 被调用方法HASH
     * @return false: 不存在 true: 存在
     */
    public boolean checkExistsNormalMethodCallByCalleeMethodHash(String calleeMethodHash) {
        if (calleeMethodHash == null) {
            throw new JavaCG2RuntimeException("参数不允许为空");
        }

        List<String> existsInstructionList = JavaCG2CallTypeEnum.getExistsInstructionList();
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CHECK_NORMAL_MC_BY_EE_HASH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select count(" + DC.MC_CALLEE_METHOD_HASH + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " and " + DC.MC_CALL_TYPE + " in " + JACGSqlUtil.genQuestionString(existsInstructionList.size());
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Object> argList = new ArrayList<>();
        argList.add(calleeMethodHash);
        argList.addAll(existsInstructionList);
        Long count = dbOperator.queryObjectOneColumn(sql, Long.class, argList.toArray());
        return count != null && count > 0;
    }

    /**
     * 查询存在实际指令的方法调用中的调用方与被调用方，使用被调用方完整类名与方法（排除继承相关的方法调用）
     * 被调用方法所在的jar包可能没有解析，因此不能从method_info表查询对应的方法
     *
     * @param calleeClassName  被调用方完整类名
     * @param calleeMethodName 被调用方方法
     * @param queryAllColumns  是否查询全部字段 true: 查询全部字段 false: 只查询主要字段
     * @return
     */
    public List<WriteDbData4MethodCall> queryNormalMethodCallByCalleeClassMethod(String calleeClassName, String calleeMethodName, boolean queryAllColumns) {
        List<String> existsInstructionList = JavaCG2CallTypeEnum.getExistsInstructionList();
        SqlKeyEnum sqlKeyEnum = queryAllColumns ? SqlKeyEnum.MC_QUERY_NORMAL_MC_BY_EECM_ALL_COLUMNS : SqlKeyEnum.MC_QUERY_NORMAL_MC_BY_EECM;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + (queryAllColumns ? JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL) :
                    JACGSqlUtil.joinColumns(DC.MC_CALLER_FULL_METHOD, DC.MC_CALLER_LINE_NUMBER, DC.MC_CALLEE_FULL_METHOD)) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLEE_METHOD_NAME + " = ?" +
                    " and " + DC.MC_CALL_TYPE + " in " + JACGSqlUtil.genQuestionString(existsInstructionList.size());
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Object> argList = new ArrayList<>();
        argList.add(dbOperWrapper.querySimpleClassName(calleeClassName));
        argList.add(calleeMethodName);
        argList.addAll(existsInstructionList);
        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, argList.toArray());
    }

    /**
     * 查询存在实际指令的方法调用，使用被调用方法HASH查询
     *
     * @param calleeFullMethod
     * @return
     */
    public List<WriteDbData4MethodCall> queryNormalMethodCallByCalleeFullMethod(String calleeFullMethod) {
        List<String> existsInstructionList = JavaCG2CallTypeEnum.getExistsInstructionList();
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_NORMAL_MC_BY_EEMH;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CALL) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_METHOD_HASH + " = ?" +
                    " and " + DC.MC_CALL_TYPE + " in " + JACGSqlUtil.genQuestionString(existsInstructionList.size());
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Object> argList = new ArrayList<>();
        argList.add(JACGUtil.genHashWithLen(calleeFullMethod));
        argList.addAll(existsInstructionList);
        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, argList.toArray());
    }

    /**
     * 根据被调用类名与方法名查询调用方信息
     *
     * @param calleeClassName
     * @param calleeMethodName
     * @return
     */
    public List<WriteDbData4MethodCall> queryCallerInfoByCallee(String calleeClassName, String calleeMethodName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLER_INFO_BY_CALLEE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + JACGSqlUtil.joinColumns(DC.MC_CALL_ID, DC.MC_CALLER_FULL_METHOD, DC.MC_CALL_FLAGS) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLEE_METHOD_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        return dbOperator.queryList(sql, WriteDbData4MethodCall.class, dbOperWrapper.querySimpleClassName(calleeClassName), calleeMethodName);
    }

    /**
     * 在方法调用表中，查找指定类包含特定关键字的被调用方法列表
     *
     * @param className         类名
     * @param methodNameKeyword 方法名关键字
     * @return
     */
    public List<String> queryCalleeMethodNameLike(String className, String methodNameKeyword) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLEE_METHODS_LIKE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLEE_METHOD_NAME + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALLEE_METHOD_NAME + " like concat('%', ?, '%')";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class, dbOperWrapper.querySimpleClassName(className), methodNameKeyword);
    }

    /**
     * 从方法调用表根据被调用类唯一类名查询指定被调用类最小的call_id
     *
     * @param calleeSimpleClassName 被调用类唯一类名
     * @return
     */
    public Integer queryMinCallIdEESCN(String calleeSimpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_MIN_CALL_ID_CALLEE_CLASS;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select min(" + DC.MC_CALL_ID + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryObjectOneColumn(sql, Integer.class, calleeSimpleClassName);
    }

    /**
     * 根据被调用类名与方法名查询对应的方法调用
     *
     * @param calleeMethodName
     * @param methodName
     * @return
     */
    public List<WriteDbData4MethodCall> queryMethodCallByCalleeClassMethod(String calleeMethodName, String methodName) {
        List<WriteDbData4MethodCall> methodCallList = new ArrayList<>();
        // 根据被调用类名与方法名查询对应的完整方法，不从method_info表查询
        List<String> fullMethodList = queryCalleeFullMethodByClassMethod(calleeMethodName, methodName);
        if (JavaCG2Util.isCollectionEmpty(fullMethodList)) {
            return methodCallList;
        }
        for (String fullMethod : fullMethodList) {
            List<WriteDbData4MethodCall> list = queryMethodCallByCalleeFullMethod(fullMethod);
            if (!JavaCG2Util.isCollectionEmpty(list)) {
                methodCallList.addAll(list);
            }
        }
        return methodCallList;
    }

    /**
     * 查找指定被调用类的被调用完整方法，使用简单类名
     *
     * @param calleeSimpleClassName
     * @return
     */
    public List<String> queryNormalCalleeFullMethodBySCN(String calleeSimpleClassName) {
        List<String> existsInstructionList = JavaCG2CallTypeEnum.getExistsInstructionList();
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_NORMAL_CALLEE_FULL_METHOD_BY_SCN;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select distinct(" + DC.MC_CALLEE_FULL_METHOD + ")" +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MC_CALL_TYPE + " in " + JACGSqlUtil.genQuestionString(existsInstructionList.size());
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Object> argList = new ArrayList<>();
        argList.add(calleeSimpleClassName);
        argList.addAll(existsInstructionList);
        List<String> list = dbOperator.queryListOneColumn(sql, String.class, argList.toArray());
        Collections.sort(list);
        return list;
    }

    /**
     * 根据被调用类的简单类名查询完整类名
     *
     * @param calleeSimpleClassName
     * @return
     */
    public String queryCalleeClassNameBySimple(String calleeSimpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_QUERY_CALLEE_CLASS_NAME_BY_SIMPLE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLEE_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLEE_SIMPLE_CLASS_NAME + " = ?" +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        String calleeFullMethod = dbOperator.queryObjectOneColumn(sql, String.class, calleeSimpleClassName);
        if (calleeFullMethod == null) {
            return null;
        }
        return JavaCG2ClassMethodUtil.getClassNameFromMethod(calleeFullMethod);
    }

    /**
     * 检查指定的方法是否存在父类/接口调用子类/实现类的情况
     *
     * @param callerFullMethod
     * @return
     */
    public boolean checkExistsSuperCallChild(String callerFullMethod) {
        List<String> superCallChildList = JavaCG2CallTypeEnum.getSuperCallChildList();
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MC_CHECK_EXISTS_SUPER_CALL_CHILD;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = "select " + DC.MC_CALLEE_FULL_METHOD +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CALL.getTableName() +
                    " where " + DC.MC_CALLER_METHOD_HASH + " = ?" +
                    " and " + DC.MC_CALL_TYPE + " in " + JACGSqlUtil.genQuestionString(superCallChildList.size()) +
                    " limit 1";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }

        List<Object> argList = new ArrayList<>();
        argList.add(JACGUtil.genHashWithLen(callerFullMethod));
        argList.addAll(superCallChildList);
        String calleeFullMethod = dbOperator.queryObjectOneColumn(sql, String.class, argList.toArray());
        return calleeFullMethod != null;
    }
}
