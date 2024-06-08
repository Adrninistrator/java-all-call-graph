package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2024/1/11
 * @description: 用于写入数据库的数据，字段的信息
 */
public class WriteDbData4FieldInfo implements BaseWriteDbData {
    public int recordId;
    public String simpleClassName;
    public String fieldName;
    public String fieldType;
    public String modifiers;
    public int primitiveType;
    public int staticFlag;
    public int finalFlag;
    public String className;

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

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }
}
