package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2024/6/19
 * @description: 用于写入数据库的数据，类的签名中的泛型信息
 */
public class WriteDbData4ClassSignatureGenericsType implements BaseWriteDbData {
    private int recordId;
    private String simpleClassName;
    private int seq;
    private String typeVariablesName;
    private String genericsExtendsClassName;
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

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getTypeVariablesName() {
        return typeVariablesName;
    }

    public void setTypeVariablesName(String typeVariablesName) {
        this.typeVariablesName = typeVariablesName;
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
