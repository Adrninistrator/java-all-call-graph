package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2022/11/18
 * @description: 用于写入数据库的数据，方法调用
 */
public class WriteDbData4MethodCall implements BaseWriteDbData {
    private int callId;
    private int enabled;
    private String callType;
    private String callerMethodHash;
    private String callerSimpleClassName;
    private String callerMethodName;
    private String callerFullMethod;
    private int callerLineNumber;
    private String callerReturnType;
    private String calleeMethodHash;
    private String calleeSimpleClassName;
    private String calleeMethodName;
    private String calleeFullMethod;
    private int calleeArrayDimensions;
    private String calleeObjType;
    private String rawReturnType;
    private String actualReturnType;
    private int callFlags = 0;
    private Integer callerJarNum;
    private Integer calleeJarNum;
    private String description;

    /**
     * 创建方法调用对象
     *
     * @param callId
     * @param enabled
     * @param callType
     * @param callerSimpleClassName
     * @param callerFullMethod
     * @param callerLineNum
     * @param callerReturnType
     * @param calleeSimpleClassName
     * @param calleeFullMethod
     * @param calleeArrayDimensions
     * @param calleeObjType
     * @param rawReturnType
     * @param actualReturnType
     * @param callerJarNum
     * @param calleeJarNum
     * @param desc
     * @return
     */
    public static WriteDbData4MethodCall genInstance(int callId,
                                                     boolean enabled,
                                                     String callType,
                                                     String callerSimpleClassName,
                                                     String callerFullMethod,
                                                     int callerLineNum,
                                                     String callerReturnType,
                                                     String calleeSimpleClassName,
                                                     String calleeFullMethod,
                                                     int calleeArrayDimensions,
                                                     String calleeObjType,
                                                     String rawReturnType,
                                                     String actualReturnType,
                                                     Integer callerJarNum,
                                                     Integer calleeJarNum,
                                                     String desc
    ) {
        String callerMethodHash = JACGUtil.genHashWithLen(callerFullMethod);
        String callerMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(callerFullMethod);

        WriteDbData4MethodCall writeDbData4MethodCall = new WriteDbData4MethodCall();
        writeDbData4MethodCall.setCallId(callId);
        writeDbData4MethodCall.setEnabled(enabled ? JavaCG2YesNoEnum.YES.getIntValue() : JavaCG2YesNoEnum.NO.getIntValue());
        writeDbData4MethodCall.setCallType(callType);
        writeDbData4MethodCall.setCalleeObjType(calleeObjType);
        writeDbData4MethodCall.setCallerMethodHash(callerMethodHash);
        writeDbData4MethodCall.setCallerSimpleClassName(callerSimpleClassName);
        writeDbData4MethodCall.setCallerMethodName(callerMethodName);
        writeDbData4MethodCall.setCallerFullMethod(callerFullMethod);
        writeDbData4MethodCall.setCallerLineNumber(callerLineNum);
        writeDbData4MethodCall.setCallerReturnType(callerReturnType);

        String calleeMethodHash = JACGUtil.genHashWithLen(calleeFullMethod);
        String calleeMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
        writeDbData4MethodCall.setCalleeMethodHash(calleeMethodHash);
        writeDbData4MethodCall.setCalleeSimpleClassName(calleeSimpleClassName);
        writeDbData4MethodCall.setCalleeMethodName(calleeMethodName);
        writeDbData4MethodCall.setCalleeFullMethod(calleeFullMethod);
        writeDbData4MethodCall.setCalleeArrayDimensions(calleeArrayDimensions);
        writeDbData4MethodCall.setRawReturnType(rawReturnType);
        writeDbData4MethodCall.setActualReturnType(actualReturnType);
        writeDbData4MethodCall.setCallerJarNum(callerJarNum);
        writeDbData4MethodCall.setCalleeJarNum(calleeJarNum);
        writeDbData4MethodCall.setDescription(desc);
        return writeDbData4MethodCall;
    }

    //
    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }

    public int getEnabled() {
        return enabled;
    }

    public void setEnabled(int enabled) {
        this.enabled = enabled;
    }

    public String getCallType() {
        return callType;
    }

    public void setCallType(String callType) {
        this.callType = callType;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }

    public String getCallerSimpleClassName() {
        return callerSimpleClassName;
    }

    public void setCallerSimpleClassName(String callerSimpleClassName) {
        this.callerSimpleClassName = callerSimpleClassName;
    }

    public String getCallerMethodName() {
        return callerMethodName;
    }

    public void setCallerMethodName(String callerMethodName) {
        this.callerMethodName = callerMethodName;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public void setCallerFullMethod(String callerFullMethod) {
        this.callerFullMethod = callerFullMethod;
    }

    public int getCallerLineNumber() {
        return callerLineNumber;
    }

    public void setCallerLineNumber(int callerLineNumber) {
        this.callerLineNumber = callerLineNumber;
    }

    public String getCallerReturnType() {
        return callerReturnType;
    }

    public void setCallerReturnType(String callerReturnType) {
        this.callerReturnType = callerReturnType;
    }

    public String getCalleeMethodHash() {
        return calleeMethodHash;
    }

    public void setCalleeMethodHash(String calleeMethodHash) {
        this.calleeMethodHash = calleeMethodHash;
    }

    public String getCalleeSimpleClassName() {
        return calleeSimpleClassName;
    }

    public void setCalleeSimpleClassName(String calleeSimpleClassName) {
        this.calleeSimpleClassName = calleeSimpleClassName;
    }

    public String getCalleeMethodName() {
        return calleeMethodName;
    }

    public void setCalleeMethodName(String calleeMethodName) {
        this.calleeMethodName = calleeMethodName;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public void setCalleeFullMethod(String calleeFullMethod) {
        this.calleeFullMethod = calleeFullMethod;
    }

    public int getCalleeArrayDimensions() {
        return calleeArrayDimensions;
    }

    public void setCalleeArrayDimensions(int calleeArrayDimensions) {
        this.calleeArrayDimensions = calleeArrayDimensions;
    }

    public String getCalleeObjType() {
        return calleeObjType;
    }

    public void setCalleeObjType(String calleeObjType) {
        this.calleeObjType = calleeObjType;
    }

    public String getRawReturnType() {
        return rawReturnType;
    }

    public void setRawReturnType(String rawReturnType) {
        this.rawReturnType = rawReturnType;
    }

    public String getActualReturnType() {
        return actualReturnType;
    }

    public void setActualReturnType(String actualReturnType) {
        this.actualReturnType = actualReturnType;
    }

    public int getCallFlags() {
        return callFlags;
    }

    public void setCallFlags(int callFlags) {
        this.callFlags = callFlags;
    }

    public Integer getCallerJarNum() {
        return callerJarNum;
    }

    public void setCallerJarNum(Integer callerJarNum) {
        this.callerJarNum = callerJarNum;
    }

    public Integer getCalleeJarNum() {
        return calleeJarNum;
    }

    public void setCalleeJarNum(Integer calleeJarNum) {
        this.calleeJarNum = calleeJarNum;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public String toString() {
        return "callId=" + callId +
                ", callerSimpleClassName='" + callerSimpleClassName + '\'' +
                ", callerMethodName='" + callerMethodName + '\'' +
                ", callerLineNumber=" + callerLineNumber +
                ", calleeSimpleClassName='" + calleeSimpleClassName + '\'' +
                ", calleeMethodName='" + calleeMethodName + '\'';
    }
}
