package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/8/9
 * @description: 用于写入数据库的数据，非静态字段中涉及的泛型类型
 */
public class WriteDbData4FieldGenericsType implements BaseWriteDbData {
    private int recordId;
    private String simpleClassName;
    private String fieldName;
    private String type;
    private int typeSeq;
    private String simpleGenericsTypeNad;
    private int genericsArrayDimensions;
    private String typeVariablesName;
    private String wildcard;
    private String referenceType;
    private String genericsCategory;
    private String genericsTypeNad;
    private String className;

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

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getTypeSeq() {
        return typeSeq;
    }

    public void setTypeSeq(int typeSeq) {
        this.typeSeq = typeSeq;
    }

    public String getSimpleGenericsTypeNad() {
        return simpleGenericsTypeNad;
    }

    public void setSimpleGenericsTypeNad(String simpleGenericsTypeNad) {
        this.simpleGenericsTypeNad = simpleGenericsTypeNad;
    }

    public int getGenericsArrayDimensions() {
        return genericsArrayDimensions;
    }

    public void setGenericsArrayDimensions(int genericsArrayDimensions) {
        this.genericsArrayDimensions = genericsArrayDimensions;
    }

    public String getTypeVariablesName() {
        return typeVariablesName;
    }

    public void setTypeVariablesName(String typeVariablesName) {
        this.typeVariablesName = typeVariablesName;
    }

    public String getWildcard() {
        return wildcard;
    }

    public void setWildcard(String wildcard) {
        this.wildcard = wildcard;
    }

    public String getReferenceType() {
        return referenceType;
    }

    public void setReferenceType(String referenceType) {
        this.referenceType = referenceType;
    }

    public String getGenericsCategory() {
        return genericsCategory;
    }

    public void setGenericsCategory(String genericsCategory) {
        this.genericsCategory = genericsCategory;
    }

    public String getGenericsTypeNad() {
        return genericsTypeNad;
    }

    public void setGenericsTypeNad(String genericsTypeNad) {
        this.genericsTypeNad = genericsTypeNad;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    @Override
    public String toString() {
        return "WriteDbData4FieldGenericsType{" +
                "recordId=" + recordId +
                ", simpleClassName='" + simpleClassName + '\'' +
                ", fieldName='" + fieldName + '\'' +
                ", type='" + type + '\'' +
                ", typeSeq=" + typeSeq +
                ", simpleGenericsType='" + simpleGenericsTypeNad + '\'' +
                ", genericsArrayDimensions=" + genericsArrayDimensions +
                ", typeVariablesName='" + typeVariablesName + '\'' +
                ", wildcard='" + wildcard + '\'' +
                ", referenceType='" + referenceType + '\'' +
                ", genericsCategory='" + genericsCategory + '\'' +
                ", genericsType='" + genericsTypeNad + '\'' +
                ", className='" + className + '\'' +
                '}';
    }
}
