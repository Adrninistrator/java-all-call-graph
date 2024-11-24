package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2024/1/11
 * @description: 用于写入数据库的数据，字段的信息
 */
public class WriteDbData4FieldInfo implements BaseWriteDbData {
    private int recordId;
    private String simpleClassName;
    private String fieldName;
    private String fieldType;
    private int arrayDimensions;
    private String fieldCategory;
    private String modifiers;
    private int primitiveType;
    private int staticFlag;
    private int finalFlag;
    private int existsGetMethod;
    private int existsSetMethod;
    private int existsGenericsType;
    private String className;

    @Override
    public String toString() {
        return "recordId=" + recordId +
                ", simpleClassName=" + simpleClassName +
                ", fieldName=" + fieldName +
                ", fieldType=" + fieldType;
    }

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
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

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public int getArrayDimensions() {
        return arrayDimensions;
    }

    public void setArrayDimensions(int arrayDimensions) {
        this.arrayDimensions = arrayDimensions;
    }

    public String getFieldCategory() {
        return fieldCategory;
    }

    public void setFieldCategory(String fieldCategory) {
        this.fieldCategory = fieldCategory;
    }

    public String getModifiers() {
        return modifiers;
    }

    public void setModifiers(String modifiers) {
        this.modifiers = modifiers;
    }

    public int getPrimitiveType() {
        return primitiveType;
    }

    public void setPrimitiveType(int primitiveType) {
        this.primitiveType = primitiveType;
    }

    public int getStaticFlag() {
        return staticFlag;
    }

    public void setStaticFlag(int staticFlag) {
        this.staticFlag = staticFlag;
    }

    public int getFinalFlag() {
        return finalFlag;
    }

    public void setFinalFlag(int finalFlag) {
        this.finalFlag = finalFlag;
    }

    public int getExistsGetMethod() {
        return existsGetMethod;
    }

    public void setExistsGetMethod(int existsGetMethod) {
        this.existsGetMethod = existsGetMethod;
    }

    public int getExistsSetMethod() {
        return existsSetMethod;
    }

    public void setExistsSetMethod(int existsSetMethod) {
        this.existsSetMethod = existsSetMethod;
    }

    public int getExistsGenericsType() {
        return existsGenericsType;
    }

    public void setExistsGenericsType(int existsGenericsType) {
        this.existsGenericsType = existsGenericsType;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }
}
