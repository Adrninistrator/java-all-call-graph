package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2024/7/16
 * @description: 用于写入数据库的数据，类的签名中继承或实现的泛型关系
 */
public class WriteDbData4ClassSigExtImplGenerics implements BaseWriteDbData {

    private int recordId;
    private String simpleClassName;
    private String genericsName;
    private int seq;
    private String extType;
    private String superItfSimpleClassName;
    private String superItfGenericsExtendsClassName;
    private int superItfSeq;
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

    public String getGenericsName() {
        return genericsName;
    }

    public void setGenericsName(String genericsName) {
        this.genericsName = genericsName;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getExtType() {
        return extType;
    }

    public void setExtType(String extType) {
        this.extType = extType;
    }

    public String getSuperItfSimpleClassName() {
        return superItfSimpleClassName;
    }

    public void setSuperItfSimpleClassName(String superItfSimpleClassName) {
        this.superItfSimpleClassName = superItfSimpleClassName;
    }

    public String getSuperItfGenericsExtendsClassName() {
        return superItfGenericsExtendsClassName;
    }

    public void setSuperItfGenericsExtendsClassName(String superItfGenericsExtendsClassName) {
        this.superItfGenericsExtendsClassName = superItfGenericsExtendsClassName;
    }

    public int getSuperItfSeq() {
        return superItfSeq;
    }

    public void setSuperItfSeq(int superItfSeq) {
        this.superItfSeq = superItfSeq;
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
