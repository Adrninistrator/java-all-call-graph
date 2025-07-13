package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/7/2
 * @description: 用于写入数据库的数据，Spring AOP advice Around信息
 */
public class WriteDbData4SpringAopAdviceAround implements BaseWriteDbData {

    private int recordId;
    private String adviceFullMethod;
    private String adviceMethodReturnType;
    private String adviceMethodHash;
    private int proceedCallId;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getAdviceFullMethod() {
        return adviceFullMethod;
    }

    public void setAdviceFullMethod(String adviceFullMethod) {
        this.adviceFullMethod = adviceFullMethod;
    }

    public String getAdviceMethodReturnType() {
        return adviceMethodReturnType;
    }

    public void setAdviceMethodReturnType(String adviceMethodReturnType) {
        this.adviceMethodReturnType = adviceMethodReturnType;
    }

    public String getAdviceMethodHash() {
        return adviceMethodHash;
    }

    public void setAdviceMethodHash(String adviceMethodHash) {
        this.adviceMethodHash = adviceMethodHash;
    }

    public int getProceedCallId() {
        return proceedCallId;
    }

    public void setProceedCallId(int proceedCallId) {
        this.proceedCallId = proceedCallId;
    }
}
