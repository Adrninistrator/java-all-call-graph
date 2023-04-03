package com.adrninistrator.jacg.dto.write_db;

import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;

/**
 * @author adrninistrator
 * @date 2022/11/18
 * @description: 用于写入数据库的数据，方法调用
 */
public class WriteDbData4MethodCall extends AbstractWriteDbData {
    private int callId;
    private String callType;
    private String calleeObjType;
    private int enabled;
    private String callerJarNum;
    private String callerMethodHash;
    private String callerSimpleClassName;
    private String callerMethodName;
    private String callerFullMethod;
    private int callerLineNumber;
    private String calleeMethodHash;
    private String calleeSimpleClassName;
    private String calleeMethodName;
    private String calleeFullMethod;
    private int callFlags = 0;
    private String rawReturnType;
    private String actualReturnType;

    /**
     * 创建方法调用对象
     *
     * @param callType
     * @param callerFullMethod
     * @param callerSimpleClassName
     * @param calleeFullMethod
     * @param calleeSimpleClassName
     * @param callId
     * @param callerLineNum
     * @param callerJarNum
     * @return
     */
    public static WriteDbData4MethodCall genInstance(String callType,
                                                     String calleeObjType,
                                                     String callerSimpleClassName,
                                                     String callerFullMethod,
                                                     String calleeSimpleClassName,
                                                     String calleeFullMethod,
                                                     int callId,
                                                     int callerLineNum,
                                                     String callerJarNum,
                                                     String rawReturnType,
                                                     String actualReturnType
    ) {
        String callerMethodHash = JACGUtil.genHashWithLen(callerFullMethod);
        String callerMethodName = JACGClassMethodUtil.getMethodNameFromFull(callerFullMethod);

        WriteDbData4MethodCall writeDbData4MethodCall = new WriteDbData4MethodCall();
        writeDbData4MethodCall.setCallId(callId);
        writeDbData4MethodCall.setCallType(callType);
        writeDbData4MethodCall.setCalleeObjType(calleeObjType);
        writeDbData4MethodCall.setEnabled(JavaCGYesNoEnum.YES.getIntValue());
        writeDbData4MethodCall.setCallerJarNum(callerJarNum);
        writeDbData4MethodCall.setCallerMethodHash(callerMethodHash);
        writeDbData4MethodCall.setCallerSimpleClassName(callerSimpleClassName);
        writeDbData4MethodCall.setCallerMethodName(callerMethodName);
        writeDbData4MethodCall.setCallerFullMethod(callerFullMethod);
        writeDbData4MethodCall.setCallerLineNumber(callerLineNum);

        String calleeMethodHash = JACGUtil.genHashWithLen(calleeFullMethod);
        String calleeMethodName = JACGClassMethodUtil.getMethodNameFromFull(calleeFullMethod);
        writeDbData4MethodCall.setCalleeMethodHash(calleeMethodHash);
        writeDbData4MethodCall.setCalleeSimpleClassName(calleeSimpleClassName);
        writeDbData4MethodCall.setCalleeMethodName(calleeMethodName);
        writeDbData4MethodCall.setCalleeFullMethod(calleeFullMethod);

        writeDbData4MethodCall.setRawReturnType(rawReturnType);
        writeDbData4MethodCall.setActualReturnType(actualReturnType);

        return writeDbData4MethodCall;
    }

    //
    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }

    public String getCallType() {
        return callType;
    }

    public void setCallType(String callType) {
        this.callType = callType;
    }

    public String getCalleeObjType() {
        return calleeObjType;
    }

    public void setCalleeObjType(String calleeObjType) {
        this.calleeObjType = calleeObjType;
    }

    public int getEnabled() {
        return enabled;
    }

    public void setEnabled(int enabled) {
        this.enabled = enabled;
    }

    public String getCallerJarNum() {
        return callerJarNum;
    }

    public void setCallerJarNum(String callerJarNum) {
        this.callerJarNum = callerJarNum;
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

    public int getCallFlags() {
        return callFlags;
    }

    public void setCallFlags(int callFlags) {
        this.callFlags = callFlags;
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
}
