package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，方法调用信息
 */
public class WriteDbData4MethodCallInfo extends AbstractWriteDbData {
    private int callId;
    private int objArgsSeq;
    private int seq;
    private String type;
    private int arrayFlag;
    private String theValue;

    public WriteDbData4MethodCallInfo() {
    }

    public WriteDbData4MethodCallInfo(int callId, int objArgsSeq, int seq, String type, int arrayFlag, String theValue) {
        this.callId = callId;
        this.objArgsSeq = objArgsSeq;
        this.seq = seq;
        this.type = type;
        this.arrayFlag = arrayFlag;
        this.theValue = theValue;
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

    public String getTheValue() {
        return theValue;
    }

    public void setTheValue(String theValue) {
        this.theValue = theValue;
    }
}
