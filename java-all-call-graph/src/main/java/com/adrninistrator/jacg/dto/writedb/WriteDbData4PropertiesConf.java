package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/9/18
 * @description: 用于写入数据库的数据，properties文件配置信息
 */
public class WriteDbData4PropertiesConf implements BaseWriteDbData {

    private int recordId;
    private String propertiesKey;
    private String propertiesFilePath;
    private String propertiesFileName;
    private String propertiesValue;

    public WriteDbData4PropertiesConf() {
    }

    public WriteDbData4PropertiesConf(int recordId, String propertiesKey, String propertiesFilePath, String propertiesFileName, String propertiesValue) {
        this.recordId = recordId;
        this.propertiesKey = propertiesKey;
        this.propertiesFilePath = propertiesFilePath;
        this.propertiesFileName = propertiesFileName;
        this.propertiesValue = propertiesValue;
    }

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getPropertiesKey() {
        return propertiesKey;
    }

    public void setPropertiesKey(String propertiesKey) {
        this.propertiesKey = propertiesKey;
    }

    public String getPropertiesFilePath() {
        return propertiesFilePath;
    }

    public void setPropertiesFilePath(String propertiesFilePath) {
        this.propertiesFilePath = propertiesFilePath;
    }

    public String getPropertiesFileName() {
        return propertiesFileName;
    }

    public void setPropertiesFileName(String propertiesFileName) {
        this.propertiesFileName = propertiesFileName;
    }

    public String getPropertiesValue() {
        return propertiesValue;
    }

    public void setPropertiesValue(String propertiesValue) {
        this.propertiesValue = propertiesValue;
    }
}
