package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/7/25
 * @description: 用于写入数据库的数据，使用其他类中字段的使用情况
 */
public class WriteDbData4FieldUsageOther implements BaseWriteDbData {
    private int recordId;
    private String fullMethod;
    private String methodReturnType;
    private int staticFlag;
    private int getOrPut;
    private String fieldInSimpleClassName;
    private String fieldName;
    private String fieldType;
    private int lineNumber;
    private String simpleClassName;
    private String className;
    private String methodHash;
    private String fieldInClassName;
    private Integer classJarNum;
    private Integer fieldJarNum;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getMethodReturnType() {
        return methodReturnType;
    }

    public void setMethodReturnType(String methodReturnType) {
        this.methodReturnType = methodReturnType;
    }

    public int getStaticFlag() {
        return staticFlag;
    }

    public void setStaticFlag(int staticFlag) {
        this.staticFlag = staticFlag;
    }

    public int getGetOrPut() {
        return getOrPut;
    }

    public void setGetOrPut(int getOrPut) {
        this.getOrPut = getOrPut;
    }

    public String getFieldInSimpleClassName() {
        return fieldInSimpleClassName;
    }

    public void setFieldInSimpleClassName(String fieldInSimpleClassName) {
        this.fieldInSimpleClassName = fieldInSimpleClassName;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(int lineNumber) {
        this.lineNumber = lineNumber;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getFieldInClassName() {
        return fieldInClassName;
    }

    public void setFieldInClassName(String fieldInClassName) {
        this.fieldInClassName = fieldInClassName;
    }

    public Integer getClassJarNum() {
        return classJarNum;
    }

    public void setClassJarNum(Integer classJarNum) {
        this.classJarNum = classJarNum;
    }

    public Integer getFieldJarNum() {
        return fieldJarNum;
    }

    public void setFieldJarNum(Integer fieldJarNum) {
        this.fieldJarNum = fieldJarNum;
    }

    @Override
    public String toString() {
        return "WriteDbData4FieldUsageOther{" +
                "recordId=" + recordId +
                ", fullMethod='" + fullMethod + '\'' +
                ", methodReturnType='" + methodReturnType + '\'' +
                ", staticFlag=" + staticFlag +
                ", getOrPut=" + getOrPut +
                ", fieldInSimpleClassName='" + fieldInSimpleClassName + '\'' +
                ", fieldName='" + fieldName + '\'' +
                ", fieldType='" + fieldType + '\'' +
                ", lineNumber=" + lineNumber +
                ", simpleClassName='" + simpleClassName + '\'' +
                ", className='" + className + '\'' +
                ", methodHash='" + methodHash + '\'' +
                ", fieldInClassName='" + fieldInClassName + '\'' +
                ", classJarNum=" + classJarNum +
                ", fieldJarNum=" + fieldJarNum +
                '}';
    }
}
