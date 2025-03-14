package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/2/24
 * @description: 用于写入数据库的数据，MyBatis XML中格式化后的sql文本（使用MySQL）
 */
public class WriteDbData4MyBatisMSFormatedSql implements BaseWriteDbData {
    private int recordId;
    private String xmlFileName;
    private String sqlId;
    private int sqlSeq;
    private String xmlElementName;
    private String formatedSql;
    private String mapperSimpleClassName;
    private String mapperClassName;
    private String xmlFilePath;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getXmlFileName() {
        return xmlFileName;
    }

    public void setXmlFileName(String xmlFileName) {
        this.xmlFileName = xmlFileName;
    }

    public String getSqlId() {
        return sqlId;
    }

    public void setSqlId(String sqlId) {
        this.sqlId = sqlId;
    }

    public int getSqlSeq() {
        return sqlSeq;
    }

    public void setSqlSeq(int sqlSeq) {
        this.sqlSeq = sqlSeq;
    }

    public String getXmlElementName() {
        return xmlElementName;
    }

    public void setXmlElementName(String xmlElementName) {
        this.xmlElementName = xmlElementName;
    }

    public String getFormatedSql() {
        return formatedSql;
    }

    public void setFormatedSql(String formatedSql) {
        this.formatedSql = formatedSql;
    }

    public String getMapperSimpleClassName() {
        return mapperSimpleClassName;
    }

    public void setMapperSimpleClassName(String mapperSimpleClassName) {
        this.mapperSimpleClassName = mapperSimpleClassName;
    }

    public String getMapperClassName() {
        return mapperClassName;
    }

    public void setMapperClassName(String mapperClassName) {
        this.mapperClassName = mapperClassName;
    }

    public String getXmlFilePath() {
        return xmlFilePath;
    }

    public void setXmlFilePath(String xmlFilePath) {
        this.xmlFilePath = xmlFilePath;
    }
}
