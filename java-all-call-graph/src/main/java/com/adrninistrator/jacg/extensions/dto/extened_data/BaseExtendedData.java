package com.adrninistrator.jacg.extensions.dto.extened_data;

/**
 * @author adrninistrator
 * @date 2021/11/8
 * @description:
 */
public class BaseExtendedData {

    // 自定义数据类型
    protected String dataType;

    // 自定义数据值
    protected String dataValue;

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
        return "BaseExtendedData{" +
                "dataType='" + dataType + '\'' +
                ", dataValue='" + dataValue + '\'' +
                '}';
    }
}
