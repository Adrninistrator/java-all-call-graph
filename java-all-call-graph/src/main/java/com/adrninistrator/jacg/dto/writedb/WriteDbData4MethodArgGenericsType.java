package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/3/20
 * @description: 用于写入数据库的数据，方法参数泛型类型
 */
public class WriteDbData4MethodArgGenericsType implements BaseWriteDbData {
    private int recordId;
    private String methodHash;
    private String simpleClassName;
    private int argSeq;
    private String type;
    private int typeSeq;
    private String simpleGenericsType;
    private String genericsType;
    private String fullMethod;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public int getArgSeq() {
        return argSeq;
    }

    public void setArgSeq(int argSeq) {
        this.argSeq = argSeq;
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

    public String getSimpleGenericsType() {
        return simpleGenericsType;
    }

    public void setSimpleGenericsType(String simpleGenericsType) {
        this.simpleGenericsType = simpleGenericsType;
    }

    public String getGenericsType() {
        return genericsType;
    }

    public void setGenericsType(String genericsType) {
        this.genericsType = genericsType;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }
}
