package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/12/2
 * @description: 用于写入数据库的数据，方法的finally信息
 */
public class WriteDbData4MethodFinally implements BaseWriteDbData {

    private int recordId;
    private String methodHash;
    private String simpleClassName;
    private String tryCatch;
    private int tryCatchStartLineNumber;
    private int tryCatchEndLineNumber;
    private int tryCatchMinCallId;
    private int tryCatchMaxCallId;
    private int finallyStartLineNumber;
    private String fullMethod;
    private String returnType;

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

    public String getTryCatch() {
        return tryCatch;
    }

    public void setTryCatch(String tryCatch) {
        this.tryCatch = tryCatch;
    }

    public int getTryCatchStartLineNumber() {
        return tryCatchStartLineNumber;
    }

    public void setTryCatchStartLineNumber(int tryCatchStartLineNumber) {
        this.tryCatchStartLineNumber = tryCatchStartLineNumber;
    }

    public int getTryCatchEndLineNumber() {
        return tryCatchEndLineNumber;
    }

    public void setTryCatchEndLineNumber(int tryCatchEndLineNumber) {
        this.tryCatchEndLineNumber = tryCatchEndLineNumber;
    }

    public int getTryCatchMinCallId() {
        return tryCatchMinCallId;
    }

    public void setTryCatchMinCallId(int tryCatchMinCallId) {
        this.tryCatchMinCallId = tryCatchMinCallId;
    }

    public int getTryCatchMaxCallId() {
        return tryCatchMaxCallId;
    }

    public void setTryCatchMaxCallId(int tryCatchMaxCallId) {
        this.tryCatchMaxCallId = tryCatchMaxCallId;
    }

    public int getFinallyStartLineNumber() {
        return finallyStartLineNumber;
    }

    public void setFinallyStartLineNumber(int finallyStartLineNumber) {
        this.finallyStartLineNumber = finallyStartLineNumber;
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
}
