package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/11/4
 * @description: 用于写入数据库的数据，方法返回值对应的方法参数序号
 */
public class WriteDbData4MethodReturnArgSeq implements BaseWriteDbData {

    private int recordId;
    private String methodHash;
    private int returnArgSeq;
    private String fullMethod;
    private String returnType;
    private int equivalentConversion;

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

    public int getReturnArgSeq() {
        return returnArgSeq;
    }

    public void setReturnArgSeq(int returnArgSeq) {
        this.returnArgSeq = returnArgSeq;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    public int getEquivalentConversion() {
        return equivalentConversion;
    }

    public void setEquivalentConversion(int equivalentConversion) {
        this.equivalentConversion = equivalentConversion;
    }
}
