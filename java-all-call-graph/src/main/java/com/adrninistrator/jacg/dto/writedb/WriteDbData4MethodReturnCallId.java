package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/10/15
 * @description: 用于写入数据库的数据，方法返回值对应的方法调用序号
 */
public class WriteDbData4MethodReturnCallId implements BaseWriteDbData {

    private String callerMethodHash;
    private int returnCallId;
    private String callerFullMethod;
    private int equivalentConversion;

    public WriteDbData4MethodReturnCallId() {
    }

    public WriteDbData4MethodReturnCallId(String callerMethodHash, int returnCallId, String callerFullMethod, int equivalentConversion) {
        this.callerMethodHash = callerMethodHash;
        this.returnCallId = returnCallId;
        this.callerFullMethod = callerFullMethod;
        this.equivalentConversion = equivalentConversion;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }

    public int getReturnCallId() {
        return returnCallId;
    }

    public void setReturnCallId(int returnCallId) {
        this.returnCallId = returnCallId;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public void setCallerFullMethod(String callerFullMethod) {
        this.callerFullMethod = callerFullMethod;
    }

    public int getEquivalentConversion() {
        return equivalentConversion;
    }

    public void setEquivalentConversion(int equivalentConversion) {
        this.equivalentConversion = equivalentConversion;
    }
}
