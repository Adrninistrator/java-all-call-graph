package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/10/15
 * @description: 用于写入数据库的数据，方法返回值对应的方法调用序号
 */
public class WriteDbData4MethodReturnCallId implements BaseWriteDbData {

    private int recordId;
    private String methodHash;
    private int returnCallId;
    private String fullMethod;
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

    public int getReturnCallId() {
        return returnCallId;
    }

    public void setReturnCallId(int returnCallId) {
        this.returnCallId = returnCallId;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public int getEquivalentConversion() {
        return equivalentConversion;
    }

    public void setEquivalentConversion(int equivalentConversion) {
        this.equivalentConversion = equivalentConversion;
    }
}
