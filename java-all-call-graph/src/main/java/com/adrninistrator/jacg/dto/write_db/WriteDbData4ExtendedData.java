package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/12/9
 * @description: 用于写入数据库的数据，方法调用自定义数据
 */
public class WriteDbData4ExtendedData extends AbstractWriteDbData {
    private final int callId;

    private final String dataType;

    private final String dataValue;

    public WriteDbData4ExtendedData(int callId, String dataType, String dataValue) {
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
