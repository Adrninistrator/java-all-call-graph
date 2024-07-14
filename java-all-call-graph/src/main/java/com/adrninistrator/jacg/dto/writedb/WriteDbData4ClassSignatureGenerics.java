package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2024/6/19
 * @description: 用于写入数据库的数据，类的签名中的泛型信息
 */
public class WriteDbData4ClassSignatureGenerics implements BaseWriteDbData {
    private int recordId;
    private String simpleClassName;
    private int seq;
    private String genericsName;
    private String genericsExtendsClassName;
    private String className;

    public WriteDbData4ClassSignatureGenerics() {
    }

    public WriteDbData4ClassSignatureGenerics(String simpleClassName, int seq, String genericsName, String genericsExtendsClassName, String className) {
        this.simpleClassName = simpleClassName;
        this.seq = seq;
        this.genericsName = genericsName;
        this.genericsExtendsClassName = genericsExtendsClassName;
        this.className = className;
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

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getGenericsName() {
        return genericsName;
    }

    public void setGenericsName(String genericsName) {
        this.genericsName = genericsName;
    }

    public String getGenericsExtendsClassName() {
        return genericsExtendsClassName;
    }

    public void setGenericsExtendsClassName(String genericsExtendsClassName) {
        this.genericsExtendsClassName = genericsExtendsClassName;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }
}
