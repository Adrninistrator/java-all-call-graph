package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/11/4
 * @description: 用于写入数据库的数据，方法返回值对应的方法参数序号
 */
public class WriteDbData4MethodReturnArgSeq implements BaseWriteDbData {

    private String callerMethodHash;
    private int returnArgSeq;
    private String callerFullMethod;
    private int equivalentConversion;

    public WriteDbData4MethodReturnArgSeq() {
    }

    public WriteDbData4MethodReturnArgSeq(String callerMethodHash, int returnArgSeq, String callerFullMethod, int equivalentConversion) {
        this.callerMethodHash = callerMethodHash;
        this.returnArgSeq = returnArgSeq;
        this.callerFullMethod = callerFullMethod;
        this.equivalentConversion = equivalentConversion;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }

    public int getReturnArgSeq() {
        return returnArgSeq;
    }

    public void setReturnArgSeq(int returnArgSeq) {
        this.returnArgSeq = returnArgSeq;
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
