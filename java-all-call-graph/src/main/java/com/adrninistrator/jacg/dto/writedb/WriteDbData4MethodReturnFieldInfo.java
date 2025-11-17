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
    private int staticField;
    private int fieldOfThis;
    private String fieldInSimpleClassName;
    private String simpleFieldTypeNad;
    private int fieldArrayDimensions;
    private String fieldName;
    private String fieldInClassName;
    private String fieldTypeNad;
    private String fullMethod;
    private String returnType;

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

    public int getStaticField() {
        return staticField;
    }

    public void setStaticField(int staticField) {
        this.staticField = staticField;
    }

    public int getFieldOfThis() {
        return fieldOfThis;
    }

    public void setFieldOfThis(int fieldOfThis) {
        this.fieldOfThis = fieldOfThis;
    }

    public String getFieldInSimpleClassName() {
        return fieldInSimpleClassName;
    }

    public void setFieldInSimpleClassName(String fieldInSimpleClassName) {
        this.fieldInSimpleClassName = fieldInSimpleClassName;
    }

    public String getSimpleFieldTypeNad() {
        return simpleFieldTypeNad;
    }

    public void setSimpleFieldTypeNad(String simpleFieldTypeNad) {
        this.simpleFieldTypeNad = simpleFieldTypeNad;
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

    public String getFieldTypeNad() {
        return fieldTypeNad;
    }

    public void setFieldTypeNad(String fieldTypeNad) {
        this.fieldTypeNad = fieldTypeNad;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }
}
