package com.adrninistrator.jacg.dto.writedb.base;

/**
 * @author adrninistrator
 * @date 2025/5/28
 * @description: 用于写入数据库的数据，方法调用使用类的字段信息
 */
public class BaseWriteDbData4MethodCallClassField implements BaseWriteDbData {
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
}
