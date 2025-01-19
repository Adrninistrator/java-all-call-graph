package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/1/8
 * @description: 用于写入数据库的数据，方法返回的字段（含枚举）
 */
public class WriteDbData4MethodReturnFieldInfo implements BaseWriteDbData {

    private int recordId;
    private String methodHash;
    private int seq;
    private boolean staticField;
    private boolean fieldOfThis;
    private String fieldInSimpleClassName;
    private String fieldSimpleType;
    private int fieldArrayDimensions;
    private String fieldName;
    private String fieldInClassName;
    private String fieldType;
    private String fullMethod;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public boolean isStaticField() {
        return staticField;
    }

    public void setStaticField(boolean staticField) {
        this.staticField = staticField;
    }

    public boolean isFieldOfThis() {
        return fieldOfThis;
    }

    public void setFieldOfThis(boolean fieldOfThis) {
        this.fieldOfThis = fieldOfThis;
    }

    public String getFieldInSimpleClassName() {
        return fieldInSimpleClassName;
    }

    public void setFieldInSimpleClassName(String fieldInSimpleClassName) {
        this.fieldInSimpleClassName = fieldInSimpleClassName;
    }

    public String getFieldSimpleType() {
        return fieldSimpleType;
    }

    public void setFieldSimpleType(String fieldSimpleType) {
        this.fieldSimpleType = fieldSimpleType;
    }

    public int getFieldArrayDimensions() {
        return fieldArrayDimensions;
    }

    public void setFieldArrayDimensions(int fieldArrayDimensions) {
        this.fieldArrayDimensions = fieldArrayDimensions;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getFieldInClassName() {
        return fieldInClassName;
    }

    public void setFieldInClassName(String fieldInClassName) {
        this.fieldInClassName = fieldInClassName;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }
}
