package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2026/6/21
 * @description: 用于写入数据库的数据，方法调用被调用对象为非静态字段时的实际类型（运行时多态）
 */
public class WriteDbData4MethodCallFieldActualType implements BaseWriteDbData {
    private int recordId;
    private String callerSimpleClassName;
    private String callerClassName;
    private String callerMethodName;
    private String callerFullMethod;
    private int callerLineNumber;
    private String fieldType;
    private String fieldName;
    private String fieldActualType;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getCallerSimpleClassName() {
        return callerSimpleClassName;
    }

    public void setCallerSimpleClassName(String callerSimpleClassName) {
        this.callerSimpleClassName = callerSimpleClassName;
    }

    public String getCallerClassName() {
        return callerClassName;
    }

    public void setCallerClassName(String callerClassName) {
        this.callerClassName = callerClassName;
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

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getFieldActualType() {
        return fieldActualType;
    }

    public void setFieldActualType(String fieldActualType) {
        this.fieldActualType = fieldActualType;
    }
}
