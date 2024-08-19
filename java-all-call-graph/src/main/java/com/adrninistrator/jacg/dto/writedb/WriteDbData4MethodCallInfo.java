package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，方法调用信息
 */
public class WriteDbData4MethodCallInfo implements BaseWriteDbData {
    private int recordId;
    private int callId;
    private int objArgsSeq;
    private int seq;
    private String callerMethodHash;
    private String type;
    private int arrayFlag;
    private String valueType;
    private String theValue;
    // 数据库中保存的原始的值内容，对应the_value字段（这个字段本身不写到数据库）
    private String origValue;

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

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getArrayFlag() {
        return arrayFlag;
    }

    public void setArrayFlag(int arrayFlag) {
        this.arrayFlag = arrayFlag;
    }

    public String getValueType() {
        return valueType;
    }

    public void setValueType(String valueType) {
        this.valueType = valueType;
    }

    public String getTheValue() {
        return theValue;
    }

    public void setTheValue(String theValue) {
        this.theValue = theValue;
    }

    public String getOrigValue() {
        return origValue;
    }

    public void setOrigValue(String origValue) {
        this.origValue = origValue;
    }

    @Override
    public String toString() {
        return "WriteDbData4MethodCallInfo{" +
                "callId=" + callId +
                ", objArgsSeq=" + objArgsSeq +
                ", seq=" + seq +
                ", callerMethodHash='" + callerMethodHash + '\'' +
                ", type='" + type + '\'' +
                ", arrayFlag=" + arrayFlag +
                ", valueType='" + valueType + '\'' +
                ", theValue='" + theValue + '\'' +
                ", origValue='" + origValue + '\'' +
                '}';
    }
}
