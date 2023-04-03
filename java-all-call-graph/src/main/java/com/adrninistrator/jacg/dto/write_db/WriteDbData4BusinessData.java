package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/12/9
 * @description: 用于写入数据库的数据，方法调用业务功能数据
 */
public class WriteDbData4BusinessData extends AbstractWriteDbData {
    private final int callId;
    private final String dataType;
    private final String dataValue;

    public WriteDbData4BusinessData(int callId, String dataType, String dataValue) {
        this.callId = callId;
        this.dataType = dataType;
        this.dataValue = dataValue;
    }

    public int getCallId() {
        return callId;
    }

    public String getDataType() {
        return dataType;
    }

    public String getDataValue() {
        return dataValue;
    }
}
