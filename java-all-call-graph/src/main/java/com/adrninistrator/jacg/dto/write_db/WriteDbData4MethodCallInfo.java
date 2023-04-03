package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，方法调用信息
 */
public class WriteDbData4MethodCallInfo extends AbstractWriteDbData {
    private final int callId;
    private final int objArgsSeq;
    private final int seq;
    private final String type;
    private final int arrayFlag;
    private final String theValue;

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

    public int getObjArgsSeq() {
        return objArgsSeq;
    }

    public int getSeq() {
        return seq;
    }

    public String getType() {
        return type;
    }

    public int getArrayFlag() {
        return arrayFlag;
    }

    public String getTheValue() {
        return theValue;
    }
}
