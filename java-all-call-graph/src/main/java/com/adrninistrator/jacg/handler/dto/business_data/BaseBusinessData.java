package com.adrninistrator.jacg.handler.dto.business_data;

/**
 * @author adrninistrator
 * @date 2021/11/8
 * @description:
 */
public class BaseBusinessData {
    // 方法调用业务功能数据类型
    protected String dataType;

    // 方法调用业务功能数据值
    protected String dataValue;

    public BaseBusinessData() {
    }

    public BaseBusinessData(String dataType, String dataValue) {
        this.dataType = dataType;
        this.dataValue = dataValue;
    }

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    public String getDataValue() {
        return dataValue;
    }

    public void setDataValue(String dataValue) {
        this.dataValue = dataValue;
    }

    @Override
    public String toString() {
        return "BaseBusinessData{" +
                "dataType='" + dataType + '\'' +
                ", dataValue='" + dataValue + '\'' +
                '}';
    }
}
