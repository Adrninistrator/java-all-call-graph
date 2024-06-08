package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2024/4/25
 * @description: 用于写入数据库的数据，方法调用使用方法调用返回值
 */
public class WriteDbData4MethodCallMethodCallReturn implements BaseWriteDbData {

    private int recordId;
    private int callId;
    private int objArgsSeq;
    private int seq;
    private int arrayFlag;
    private int useReturnCallId;
    private String calleeMethodHash;
    private String calleeSimpleClassName;
    private String calleeMethodName;
    private String calleeFullMethod;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }

    public int getObjArgsSeq() {
        return objArgsSeq;
    }

    public void setObjArgsSeq(int objArgsSeq) {
        this.objArgsSeq = objArgsSeq;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public int getArrayFlag() {
        return arrayFlag;
    }

    public void setArrayFlag(int arrayFlag) {
        this.arrayFlag = arrayFlag;
    }

    public int getUseReturnCallId() {
        return useReturnCallId;
    }

    public void setUseReturnCallId(int useReturnCallId) {
        this.useReturnCallId = useReturnCallId;
    }

    public String getCalleeMethodHash() {
        return calleeMethodHash;
    }

    public void setCalleeMethodHash(String calleeMethodHash) {
        this.calleeMethodHash = calleeMethodHash;
    }

    public String getCalleeSimpleClassName() {
        return calleeSimpleClassName;
    }

    public void setCalleeSimpleClassName(String calleeSimpleClassName) {
        this.calleeSimpleClassName = calleeSimpleClassName;
    }

    public String getCalleeMethodName() {
        return calleeMethodName;
    }

    public void setCalleeMethodName(String calleeMethodName) {
        this.calleeMethodName = calleeMethodName;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public void setCalleeFullMethod(String calleeFullMethod) {
        this.calleeFullMethod = calleeFullMethod;
    }
}
