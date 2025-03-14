package com.adrninistrator.jacg.dto.methodcall;

import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.handler.dto.genericstype.GenericsTypeValue;
import com.adrninistrator.jacg.handler.dto.genericstype.MethodArgGenericsTypeInfo;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/2/25
 * @description: 方法调用链当前行的数据
 */
public abstract class MethodCallLineData {

    // 方法调用层级
    private final int methodCallLevel;

    // 调用方唯一类名
    private final String callerSimpleClassName;

    // 调用方法代码行号
    private final Integer callerLineNumber;

    // 以下为调用方（向上的方法调用）或被调用方法（向下的方法调用）相关数据
    // 实际（替换后的）的完整方法
    private final String actualFullMethod;

    // 实际（替换后的）的方法详情
    private final MethodDetail actualMethodDetail;

    // 实际（替换后的）方法HASH+长度
    private final String rawMethodHash;

    // 原始（替换前的）方法HASH+长度
    private final String actualMethodHash;

    // 方法返回类型
    private final String methodReturnType;
    // 调用方（向上的方法调用）或被调用方法（向下的方法调用）相关数据-结束

    // 方法调用ID
    private final int methodCallId;

    // 方法调用标志
    private final Integer callFlags;

    // 方法调用类型
    private final String callType;

    /*
        方法上的注解Map
        外层key:  注解类名
            内层key:      注解属性名
            内层value:    注解属性值
     */
    private Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap;

    // 方法参数泛型类型
    private MethodArgGenericsTypeInfo methodArgGenericsTypeInfo;

    // 方法返回泛型类型
    private GenericsTypeValue methodReturnGenericsTypeInfo;

    // 方法调用业务功能数据类型
    private String businessDataType;

    // 方法调用业务功能数据值
    private String businessDataValue;

    // 方法循环调用标志，若非null代表循环调用方法对应的层级
    private Integer cycleCallLevel;

    // 是否在其他线程执行
    private boolean runInOtherThread;

    // 是否在事务中执行
    private boolean runInTransaction;

    public MethodCallLineData(int methodCallLevel, String callerSimpleClassName, Integer callerLineNumber, String actualFullMethod, String rawMethodHash, String actualMethodHash
            , String methodReturnType, int methodCallId, Integer callFlags, String callType) {
        this.methodCallLevel = methodCallLevel;
        this.callerSimpleClassName = callerSimpleClassName;
        this.callerLineNumber = callerLineNumber;
        this.actualFullMethod = actualFullMethod;
        this.rawMethodHash = rawMethodHash;
        this.actualMethodHash = actualMethodHash;
        this.methodReturnType = methodReturnType;
        this.methodCallId = methodCallId;
        this.callFlags = callFlags;
        this.callType = callType;

        actualMethodDetail = JACGClassMethodUtil.genMethodDetail(actualFullMethod);
    }

    public int getMethodCallLevel() {
        return methodCallLevel;
    }

    public String getCallerSimpleClassName() {
        return callerSimpleClassName;
    }

    public Integer getCallerLineNumber() {
        return callerLineNumber;
    }

    public String getActualFullMethod() {
        return actualFullMethod;
    }

    public MethodDetail getActualMethodDetail() {
        return actualMethodDetail;
    }

    public String getRawMethodHash() {
        return rawMethodHash;
    }

    public String getActualMethodHash() {
        return actualMethodHash;
    }

    public String getMethodReturnType() {
        return methodReturnType;
    }

    public int getMethodCallId() {
        return methodCallId;
    }

    public Integer getCallFlags() {
        return callFlags;
    }

    public String getCallType() {
        return callType;
    }

    public Map<String, Map<String, BaseAnnotationAttribute>> getMethodAnnotationMap() {
        return methodAnnotationMap;
    }

    public void setMethodAnnotationMap(Map<String, Map<String, BaseAnnotationAttribute>> methodAnnotationMap) {
        this.methodAnnotationMap = methodAnnotationMap;
    }

    public MethodArgGenericsTypeInfo getMethodArgGenericsTypeInfo() {
        return methodArgGenericsTypeInfo;
    }

    public void setMethodArgGenericsTypeInfo(MethodArgGenericsTypeInfo methodArgGenericsTypeInfo) {
        this.methodArgGenericsTypeInfo = methodArgGenericsTypeInfo;
    }

    public GenericsTypeValue getMethodReturnGenericsTypeInfo() {
        return methodReturnGenericsTypeInfo;
    }

    public void setMethodReturnGenericsTypeInfo(GenericsTypeValue methodReturnGenericsTypeInfo) {
        this.methodReturnGenericsTypeInfo = methodReturnGenericsTypeInfo;
    }

    public String getBusinessDataType() {
        return businessDataType;
    }

    public void setBusinessDataType(String businessDataType) {
        this.businessDataType = businessDataType;
    }

    public String getBusinessDataValue() {
        return businessDataValue;
    }

    public void setBusinessDataValue(String businessDataValue) {
        this.businessDataValue = businessDataValue;
    }

    public Integer getCycleCallLevel() {
        return cycleCallLevel;
    }

    public void setCycleCallLevel(Integer cycleCallLevel) {
        this.cycleCallLevel = cycleCallLevel;
    }

    public boolean isRunInOtherThread() {
        return runInOtherThread;
    }

    public void setRunInOtherThread(boolean runInOtherThread) {
        this.runInOtherThread = runInOtherThread;
    }

    public boolean isRunInTransaction() {
        return runInTransaction;
    }

    public void setRunInTransaction(boolean runInTransaction) {
        this.runInTransaction = runInTransaction;
    }
}
