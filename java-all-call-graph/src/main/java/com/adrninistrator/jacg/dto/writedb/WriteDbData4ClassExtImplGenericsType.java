package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2024/7/16
 * @description: 用于写入数据库的数据，类的签名中继承或实现的泛型关系
 */
public class WriteDbData4ClassExtImplGenericsType implements BaseWriteDbData {

    private int recordId;
    private String simpleClassName;
    private String extType;
    private int seq;
    private String superItfSimpleClassName;
    private int genericsSeq;
    private String simpleGenericsTypeNad;
    private int genericsArrayDimensions;
    private String typeVariablesName;
    private String genericsCategory;
    private String genericsTypeNad;
    private String className;
    private String superItfClassName;

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

    public String getExtType() {
        return extType;
    }

    public void setExtType(String extType) {
        this.extType = extType;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getSuperItfSimpleClassName() {
        return superItfSimpleClassName;
    }

    public void setSuperItfSimpleClassName(String superItfSimpleClassName) {
        this.superItfSimpleClassName = superItfSimpleClassName;
    }

    public int getGenericsSeq() {
        return genericsSeq;
    }

    public void setGenericsSeq(int genericsSeq) {
        this.genericsSeq = genericsSeq;
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

    public String getGenericsCategory() {
        return genericsCategory;
    }

    public void setGenericsCategory(String genericsCategory) {
        this.genericsCategory = genericsCategory;
    }

    public String getGenericsTypeNad() {
        return genericsTypeNad;
    }

    public void setGenericsTypeNad(String genericsType) {
        this.genericsTypeNad = genericsType;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getSuperItfClassName() {
        return superItfClassName;
    }

    public void setSuperItfClassName(String superItfClassName) {
        this.superItfClassName = superItfClassName;
    }
}
