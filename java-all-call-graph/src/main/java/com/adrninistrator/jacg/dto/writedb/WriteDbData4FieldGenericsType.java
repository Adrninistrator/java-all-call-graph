package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/8/9
 * @description: 用于写入数据库的数据，dto的非静态字段集合中涉及的泛型类型
 */
public class WriteDbData4FieldGenericsType implements BaseWriteDbData {
    public int recordId;
    public String simpleClassName;
    public String fieldName;
    public int seq;
    public String fieldCategory;
    public String simpleFieldGenericsType;
    public String fieldGenericsType;
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

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getFieldCategory() {
        return fieldCategory;
    }

    public void setFieldCategory(String fieldCategory) {
        this.fieldCategory = fieldCategory;
    }

    public String getSimpleFieldGenericsType() {
        return simpleFieldGenericsType;
    }

    public void setSimpleFieldGenericsType(String simpleFieldGenericsType) {
        this.simpleFieldGenericsType = simpleFieldGenericsType;
    }

    public String getFieldGenericsType() {
        return fieldGenericsType;
    }

    public void setFieldGenericsType(String fieldGenericsType) {
        this.fieldGenericsType = fieldGenericsType;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }
}
