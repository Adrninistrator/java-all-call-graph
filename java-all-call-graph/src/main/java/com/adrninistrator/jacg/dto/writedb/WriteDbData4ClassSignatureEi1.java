package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description: 用于写入数据库的数据，类的签名中涉及继承与实现的信息1
 */
public class WriteDbData4ClassSignatureEi1 implements BaseWriteDbData {
    private int recordId;
    private String simpleClassName;
    private String type;
    private String superItfClassName;
    private int seq;
    private String signClassName;
    private String signGenericsName;
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

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getSuperItfClassName() {
        return superItfClassName;
    }

    public void setSuperItfClassName(String superItfClassName) {
        this.superItfClassName = superItfClassName;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getSignClassName() {
        return signClassName;
    }

    public void setSignClassName(String signClassName) {
        this.signClassName = signClassName;
    }

    public String getSignGenericsName() {
        return signGenericsName;
    }

    public void setSignGenericsName(String signGenericsName) {
        this.signGenericsName = signGenericsName;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }
}
