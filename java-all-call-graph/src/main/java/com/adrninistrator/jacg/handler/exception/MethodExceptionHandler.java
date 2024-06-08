package com.adrninistrator.jacg.handler.exception;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.SqlKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.method.ClassAndMethodName;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCatch;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodThrow;
import com.adrninistrator.jacg.handler.base.BaseHandler;
import com.adrninistrator.jacg.handler.dto.exception.BaseMethodCatchExceptionUsage;
import com.adrninistrator.jacg.handler.dto.exception.MCEU4MethodCallUseE;
import com.adrninistrator.jacg.handler.dto.exception.MCEU4MethodCallUseEMCReturn;
import com.adrninistrator.jacg.handler.dto.exception.MCEU4ThrowE;
import com.adrninistrator.jacg.handler.dto.exception.MCEU4ThrowMCUseE;
import com.adrninistrator.jacg.handler.dto.exception.MCEU4ThrowMCUseEMCReturn;
import com.adrninistrator.jacg.handler.dto.exception.NoMCEU4MethodCall;
import com.adrninistrator.jacg.handler.dto.exception.NoMCEU4MethodThrow;
import com.adrninistrator.jacg.handler.dto.exception.NoMCEU4Nothing;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallInfoHandler;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGSqlUtil;
import com.adrninistrator.javacg.common.JavaCGCommonNameConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGMethodCallInfoTypeEnum;
import com.adrninistrator.javacg.util.JavaCGUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/1/2
 * @description: 方法异常处理相关的处理类
 */
public class MethodExceptionHandler extends BaseHandler {
    private final MethodCallHandler methodCallHandler;
    private final MethodCallInfoHandler methodCallInfoHandler;

    public MethodExceptionHandler(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    public MethodExceptionHandler(DbOperWrapper dbOperWrapper) {
        super(dbOperWrapper);
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodCallInfoHandler = new MethodCallInfoHandler(dbOperWrapper);
    }

    /**
     * 通过方法catch的异常类型（唯一类名）查询对应的catch信息
     *
     * @param simpleCatchExceptionType
     * @return
     */
    public List<WriteDbData4MethodCatch> queryMethodCatchBySimpleCatchExceptionType(String simpleCatchExceptionType) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCTH_QUERY_BY_TYPE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CATCH) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CATCH.getTableName() +
                    " where " + DC.MCTH_SIMPLE_CATCH_EXCEPTION_TYPE + " = ?";
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCatch.class, dbOperWrapper.getSimpleClassName(simpleCatchExceptionType));
    }

    /**
     * 查询方法catch信息表中所有的简单类名
     *
     * @return
     */
    public List<String> queryMethodCatchSimpleClassNameList() {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCTH_QUERY_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select distinct " + DC.MCTH_SIMPLE_CLASS_NAME +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CATCH.getTableName() +
                    " order by " + DC.MCTH_SIMPLE_CLASS_NAME;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryListOneColumn(sql, String.class);
    }

    /**
     * 查询指定简单类名的方法catch信息
     *
     * @param simpleClassName
     * @return
     */
    public List<WriteDbData4MethodCatch> queryMethodCatchListBySimpleClassName(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCTH_QUERY_BY_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CATCH) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CATCH.getTableName() +
                    " where " + DC.MCTH_SIMPLE_CLASS_NAME + " = ?" +
                    " order by " + DC.MCTH_RECORD_ID;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCatch.class, simpleClassName);
    }

    /**
     * 查询指定简单类名的方法catch信息，排除编译器为switch、try-with-resource等生成的catch
     *
     * @param simpleClassName
     * @return
     */
    public List<WriteDbData4MethodCatch> queryMethodCatchListBySimpleClassNameExcludeFlag(String simpleClassName) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MCTH_QUERY_BY_SIMPLE_CLASS_NAME;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_CATCH) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_CATCH.getTableName() +
                    " where " + DC.MCTH_SIMPLE_CLASS_NAME + " = ?" +
                    " and " + DC.MCTH_CATCH_FLAG + " = ''" +
                    " order by " + DC.MCTH_RECORD_ID;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodCatch.class, simpleClassName);
    }

    /**
     * 查询方法中throw异常的信息，查询条件为方法HASH+长度，与抛出的异常对象对应的catch代码块开始指令
     *
     * @param methodHash
     * @param catchStartOffset
     * @return
     */
    public List<WriteDbData4MethodThrow> queryMethodThrowByMethodHashCatchStartOffset(String methodHash, int catchStartOffset) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MT_QUERY_BY_METHOD_HASH_CATCH_START_OFFSET;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_THROW) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_THROW.getTableName() +
                    " where " + DC.MT_METHOD_HASH + " = ?" +
                    " and " + DC.MT_CATCH_START_OFFSET + " = ?" +
                    " order by " + DC.MT_RECORD_ID;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodThrow.class, methodHash, catchStartOffset);
    }

    /**
     * 查询方法中throw异常的信息，查询条件为方法HASH+长度，与throw指令偏移量开始与结束值
     *
     * @param methodHash
     * @param startThrowOffset
     * @param endThrowOffset
     * @return
     */
    public List<WriteDbData4MethodThrow> queryMethodThrowByMethodHashThrowOffsetRange(String methodHash, int startThrowOffset, int endThrowOffset) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MT_QUERY_BY_METHOD_HASH_THROW_OFFSET_RANGE;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_THROW) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_THROW.getTableName() +
                    " where " + DC.MT_METHOD_HASH + " = ?" +
                    " and " + DC.MT_THROW_OFFSET + " >= ?" +
                    " and " + DC.MT_THROW_OFFSET + " <= ?" +
                    " order by " + DC.MT_RECORD_ID;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodThrow.class, methodHash, startThrowOffset, endThrowOffset);
    }

    /**
     * 查询方法中throw异常的信息，查询条件为方法HASH+长度，与抛出的异常对象对应的方法调用ID（抛出方法调用返回值）
     *
     * @param methodHash
     * @param callId
     * @return
     */
    public List<WriteDbData4MethodThrow> queryMethodThrowByMethodHashCallId(String methodHash, int callId) {
        SqlKeyEnum sqlKeyEnum = SqlKeyEnum.MT_QUERY_BY_METHOD_HASH_CALL_ID;
        String sql = dbOperWrapper.getCachedSql(sqlKeyEnum);
        if (sql == null) {
            sql = " select " + JACGSqlUtil.getTableAllColumns(DbTableInfoEnum.DTIE_METHOD_THROW) +
                    " from " + DbTableInfoEnum.DTIE_METHOD_THROW.getTableName() +
                    " where " + DC.MT_METHOD_HASH + " = ?" +
                    " and " + DC.MT_CALL_ID + " = ?" +
                    " order by " + DC.MT_RECORD_ID;
            sql = dbOperWrapper.cacheSql(sqlKeyEnum, sql);
        }
        return dbOperator.queryList(sql, WriteDbData4MethodThrow.class, methodHash, callId);
    }

    /**
     * 查询指定的方法catch的异常对象信息的使用情况
     *
     * @param methodCatch
     * @param expectedClassAndMethodNameList
     */
    public List<BaseMethodCatchExceptionUsage> queryMethodCatchExceptionUsage(WriteDbData4MethodCatch methodCatch, List<ClassAndMethodName> expectedClassAndMethodNameList) {
        List<BaseMethodCatchExceptionUsage> methodCatchExceptionUsageList = new ArrayList<>();
        // 处理catch的异常对象在throw中的使用情况
        handleMethodCatchExceptionThrow(methodCatch, methodCatchExceptionUsageList);

        // 查询当前catch的异常对象在方法调用中被使用的情况
        List<WriteDbData4MethodCallInfo> catchExceptionMethodCallInfoList = methodCallInfoHandler.queryMethodCallInfo4CallerByTypeValue(methodCatch.getMethodHash(),
                String.valueOf(methodCatch.getCatchStartOffset()), JavaCGMethodCallInfoTypeEnum.MCIT_METHOD_CATCH_EXCEPTION_FROM_OFFSET.getType());
        if (!JavaCGUtil.isCollectionEmpty(catchExceptionMethodCallInfoList)) {
            for (WriteDbData4MethodCallInfo methodCallInfo : catchExceptionMethodCallInfoList) {
                // 处理catch的异常对象在方法调用中的使用情况
                handleMethodCatchExceptionMethodCall(methodCatch, methodCallInfo, methodCatchExceptionUsageList, expectedClassAndMethodNameList);
            }
        }

        if (methodCatchExceptionUsageList.isEmpty()) {
            // 未查询到catch的异常对象的使用情况
            // 查询catch代码块中调用的方法
            if (methodCatch.getCatchMinCallId() >= JavaCGConstants.METHOD_CALL_ID_MIN) {
                for (int catchCallId = methodCatch.getCatchMinCallId(); catchCallId <= methodCatch.getCatchMaxCallId(); catchCallId++) {
                    WriteDbData4MethodCall methodCall = methodCallHandler.getMethodCallByCallId(catchCallId);
                    NoMCEU4MethodCall noMCEU4MethodCall = new NoMCEU4MethodCall();
                    if (JACGClassMethodUtil.checkMethodInList(methodCall.getCalleeFullMethod(), expectedClassAndMethodNameList)) {
                        noMCEU4MethodCall.setUseEInExpectedMethodCall(true);
                    }
                    noMCEU4MethodCall.setCallerLineNumber(methodCall.getCallerLineNumber());
                    noMCEU4MethodCall.setCalleeFullMethod(methodCall.getCalleeFullMethod());
                    methodCatchExceptionUsageList.add(noMCEU4MethodCall);
                }
            }
            // 查询catch代码块中通过throw抛出的异常
            List<WriteDbData4MethodThrow> methodThrowList = queryMethodThrowByMethodHashThrowOffsetRange(methodCatch.getMethodHash(), methodCatch.getCatchStartOffset(),
                    methodCatch.getCatchEndOffset());
            if (!JavaCGUtil.isCollectionEmpty(methodThrowList)) {
                for (WriteDbData4MethodThrow methodThrow : methodThrowList) {
                    NoMCEU4MethodThrow noMCEU4MethodThrow = new NoMCEU4MethodThrow();
                    noMCEU4MethodThrow.setThrowLineNumber(methodThrow.getLineNumber());
                    noMCEU4MethodThrow.setThrowExceptionType(methodThrow.getThrowExceptionType());
                    methodCatchExceptionUsageList.add(noMCEU4MethodThrow);
                }
            }
        }
        if (methodCatchExceptionUsageList.isEmpty()) {
            // 方法中catch的异常对象未被使用，且catch代码块中未找到方法调用或throw抛出异常
            methodCatchExceptionUsageList.add(new NoMCEU4Nothing());
        }
        return methodCatchExceptionUsageList;
    }

    // 处理catch的异常对象在方法调用中的使用情况
    private void handleMethodCatchExceptionMethodCall(WriteDbData4MethodCatch methodCatch, WriteDbData4MethodCallInfo methodCallInfo,
                                                      List<BaseMethodCatchExceptionUsage> methodCatchExceptionUsageList, List<ClassAndMethodName> expectedClassAndMethodNameList) {
        int callId = methodCallInfo.getCallId();
        if (callId < methodCatch.getCatchMinCallId() || callId > methodCatch.getCatchMaxCallId()) {
            // 假如当前的方法调用ID不在catch代码块的范围之内则不处理
            return;
        }

        // 查询当前catch的异常对应变量名称
        WriteDbData4MethodCallInfo methodCallInfo4NameOfVariable = methodCallInfoHandler.queryMethodCallInfoByCallIdType(methodCallInfo.getCallId(),
                methodCallInfo.getObjArgsSeq(), methodCallInfo.getSeq(), JavaCGMethodCallInfoTypeEnum.MCIT_NAME_OF_VARIABLE.getType());
        String catchExceptionVariableName = (methodCallInfo4NameOfVariable == null ? "" : methodCallInfo4NameOfVariable.getTheValue());

        WriteDbData4MethodCall methodCall = methodCallHandler.getMethodCallByCallId(callId);
        if (JavaCGConstants.METHOD_CALL_OBJECT_SEQ == methodCallInfo.getObjArgsSeq()) {
            // catch的异常对象是方法调用中的被调用对象
            if (!JavaCGCommonNameConstants.RETURN_TYPE_VOID.equals(methodCall.getRawReturnType())) {
                // 被调用方法返回值非void，查询在方法调用中使用catch的异常对象的方法调用返回值作为被调用对象或参数的情况
                List<WriteDbData4MethodCallInfo> useEReturnMethodCallInfoList =
                        methodCallInfoHandler.queryMethodCallInfo4CallerByMethodCallOrArg(methodCallInfo.getCallerMethodHash(), true, callId);
                for (WriteDbData4MethodCallInfo useEReturnMethodCallInfo : useEReturnMethodCallInfoList) {
                    // 查询使用catch的异常对象的方法调用返回值作为被调用对象或参数的方法调用
                    WriteDbData4MethodCall useEReturnMethodCall = methodCallHandler.getMethodCallByCallId(useEReturnMethodCallInfo.getCallId());

                    // 查询在方法调用中使用catch的异常对象的方法调用返回值作为被调用对象或参数，使用该方法调用返回值进行throw的情况
                    List<WriteDbData4MethodThrow> methodThrowList = queryMethodThrowByMethodHashCallId(methodCatch.getMethodHash(), useEReturnMethodCall.getCallId());
                    if (!JavaCGUtil.isCollectionEmpty(methodThrowList)) {
                        // 在方法调用中使用catch的异常对象的方法调用返回值作为被调用对象或参数，使用该方法调用返回值进行throw
                        for (WriteDbData4MethodThrow methodThrow : methodThrowList) {
                            MCEU4ThrowMCUseEMCReturn mceu4ThrowMCUseEMCReturn = new MCEU4ThrowMCUseEMCReturn();
                            mceu4ThrowMCUseEMCReturn.setCatchExceptionVariableName(catchExceptionVariableName);
                            mceu4ThrowMCUseEMCReturn.setThrowCalleeFullMethod(useEReturnMethodCall.getCalleeFullMethod());
                            mceu4ThrowMCUseEMCReturn.setThrowExceptionObjArgSeq(useEReturnMethodCallInfo.getObjArgsSeq());
                            mceu4ThrowMCUseEMCReturn.setThrowLineNumber(methodThrow.getLineNumber());
                            mceu4ThrowMCUseEMCReturn.setUseECallerLineNumber(methodCall.getCallerLineNumber());
                            mceu4ThrowMCUseEMCReturn.setUseECalleeFullMethod(methodCall.getCalleeFullMethod());
                            mceu4ThrowMCUseEMCReturn.setUseEObjArgSeq(methodCallInfo.getObjArgsSeq());
                            methodCatchExceptionUsageList.add(mceu4ThrowMCUseEMCReturn);
                        }
                        continue;
                    }

                    // 使用catch的异常对象的方法调用返回值作为被调用对象或参数的方法调用
                    MCEU4MethodCallUseEMCReturn mceu4MethodCallUseEMCReturn = new MCEU4MethodCallUseEMCReturn();
                    if (JACGClassMethodUtil.checkMethodInList(methodCall.getCalleeFullMethod(), expectedClassAndMethodNameList)) {
                        mceu4MethodCallUseEMCReturn.setUseEInExpectedMethodCall(true);
                    }
                    mceu4MethodCallUseEMCReturn.setCatchExceptionVariableName(catchExceptionVariableName);
                    mceu4MethodCallUseEMCReturn.setUseECallerLineNumber(methodCall.getCallerLineNumber());
                    mceu4MethodCallUseEMCReturn.setUseECalleeFullMethod(methodCall.getCalleeFullMethod());
                    mceu4MethodCallUseEMCReturn.setUseEObjArgSeq(methodCallInfo.getObjArgsSeq());
                    mceu4MethodCallUseEMCReturn.setUseEReturnCallerLineNumber(useEReturnMethodCall.getCallerLineNumber());
                    mceu4MethodCallUseEMCReturn.setUseEReturnCalleeFullMethod(useEReturnMethodCall.getCalleeFullMethod());
                    mceu4MethodCallUseEMCReturn.setUseEReturnObjArgSeq(useEReturnMethodCallInfo.getObjArgsSeq());
                    methodCatchExceptionUsageList.add(mceu4MethodCallUseEMCReturn);
                }
                return;
            }

            // 被调用方法返回值为void
            MCEU4MethodCallUseE mceu4MethodCallUseE = new MCEU4MethodCallUseE();
            if (JACGClassMethodUtil.checkMethodInList(methodCall.getCalleeFullMethod(), expectedClassAndMethodNameList)) {
                mceu4MethodCallUseE.setUseEInExpectedMethodCall(true);
            }
            mceu4MethodCallUseE.setCatchExceptionVariableName(catchExceptionVariableName);
            mceu4MethodCallUseE.setUseECallerLineNumber(methodCall.getCallerLineNumber());
            mceu4MethodCallUseE.setUseECalleeFullMethod(methodCall.getCalleeFullMethod());
            mceu4MethodCallUseE.setUseEObjArgSeq(methodCallInfo.getObjArgsSeq());
            methodCatchExceptionUsageList.add(mceu4MethodCallUseE);
            return;
        }

        // catch的异常对象是方法调用中的参数
        // 查询将当前方法调用返回值通过throw抛出异常的信息
        List<WriteDbData4MethodThrow> methodThrowList = queryMethodThrowByMethodHashCallId(methodCatch.getMethodHash(), callId);
        if (!JavaCGUtil.isCollectionEmpty(methodThrowList)) {
            // 当前方法调用返回值通过throw抛出异常
            for (WriteDbData4MethodThrow methodThrow : methodThrowList) {
                MCEU4ThrowMCUseE mceu4ThrowMCUseE = new MCEU4ThrowMCUseE();
                mceu4ThrowMCUseE.setCatchExceptionVariableName(catchExceptionVariableName);
                mceu4ThrowMCUseE.setThrowLineNumber(methodThrow.getLineNumber());
                mceu4ThrowMCUseE.setThrowCalleeFullMethod(methodCall.getCalleeFullMethod());
                mceu4ThrowMCUseE.setThrowExceptionObjArgSeq(methodCallInfo.getObjArgsSeq());
                methodCatchExceptionUsageList.add(mceu4ThrowMCUseE);
            }
            return;
        }

        // 当前方法调用返回值没有通过throw抛出异常
        MCEU4MethodCallUseE mceu4MethodCallUseE = new MCEU4MethodCallUseE();
        if (JACGClassMethodUtil.checkMethodInList(methodCall.getCalleeFullMethod(), expectedClassAndMethodNameList)) {
            mceu4MethodCallUseE.setUseEInExpectedMethodCall(true);
        }
        mceu4MethodCallUseE.setCatchExceptionVariableName(catchExceptionVariableName);
        mceu4MethodCallUseE.setUseECallerLineNumber(methodCall.getCallerLineNumber());
        mceu4MethodCallUseE.setUseECalleeFullMethod(methodCall.getCalleeFullMethod());
        mceu4MethodCallUseE.setUseEObjArgSeq(methodCallInfo.getObjArgsSeq());
        methodCatchExceptionUsageList.add(mceu4MethodCallUseE);
    }

    // 处理catch的异常对象在throw中的使用情况
    private void handleMethodCatchExceptionThrow(WriteDbData4MethodCatch methodCatch, List<BaseMethodCatchExceptionUsage> methodCatchExceptionUsageList) {
        // 查询方法中throw异常的信息，查询条件为方法HASH+长度，与抛出的异常对象对应的catch代码块开始指令
        List<WriteDbData4MethodThrow> methodThrowList = queryMethodThrowByMethodHashCatchStartOffset(methodCatch.getMethodHash(), methodCatch.getCatchStartOffset());
        if (JavaCGUtil.isCollectionEmpty(methodThrowList)) {
            return;
        }
        for (WriteDbData4MethodThrow methodThrow : methodThrowList) {
            MCEU4ThrowE mceu4ThrowE = new MCEU4ThrowE();
            mceu4ThrowE.setCatchExceptionVariableName(methodThrow.getCatchExceptionVariableName());
            mceu4ThrowE.setThrowLineNumber(methodThrow.getLineNumber());
            methodCatchExceptionUsageList.add(mceu4ThrowE);
        }
    }
}
