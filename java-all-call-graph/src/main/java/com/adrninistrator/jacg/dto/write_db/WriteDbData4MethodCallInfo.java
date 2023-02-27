package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，方法调用信息
 */
public class WriteDbData4MethodCallInfo extends AbstractWriteDbData {
    private final int callId;

    private final int objArgsSeq;

    private final String type;

    private final int seq;

    private final String theValue;

    public WriteDbData4MethodCallInfo(int callId, int objArgsSeq, String type, int seq, String theValue) {
        this.callId = callId;
        this.objArgsSeq = objArgsSeq;
        this.type = type;
        this.seq = seq;
        this.theValue = theValue;
    }

    public int getCallId() {
        return callId;
    }

    public int getObjArgsSeq() {
        return objArgsSeq;
    }

    public String getType() {
        return type;
    }

    public int getSeq() {
        return seq;
    }

    public String getTheValue() {
        return theValue;
    }
}
