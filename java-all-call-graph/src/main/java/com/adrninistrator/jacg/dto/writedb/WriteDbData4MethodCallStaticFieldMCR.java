package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/6/1
 * @description: 用于写入数据库的数据，方法调用使用静态字段方法调用返回值
 */
public class WriteDbData4MethodCallStaticFieldMCR implements BaseWriteDbData {

    private int recordId;
    private int callId;
    private int objArgsSeq;
    private int seq;
    private String callerMethodHash;
    private String simpleClassName;
    private String fieldName;
    private String simpleFieldType;
    private String className;
    private String fieldType;
    private String calleeMethodHash;
    private String calleeMethodName;
    private String calleeFullMethod;
    private String calleeReturnType;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }

    public int getObjArgsSeq() {
        return objArgsSeq;
    }

    public void setObjArgsSeq(int objArgsSeq) {
        this.objArgsSeq = objArgsSeq;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getSimpleFieldType() {
        return simpleFieldType;
    }

    public void setSimpleFieldType(String simpleFieldType) {
        this.simpleFieldType = simpleFieldType;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public String getCalleeMethodHash() {
        return calleeMethodHash;
    }

    public void setCalleeMethodHash(String calleeMethodHash) {
        this.calleeMethodHash = calleeMethodHash;
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

    public String getCalleeReturnType() {
        return calleeReturnType;
    }

    public void setCalleeReturnType(String calleeReturnType) {
        this.calleeReturnType = calleeReturnType;
    }
}
