package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/10/12
 * @description: 用于写入数据库的数据，MyBatis的XML中select的字段信息（使用MySQL）
 */
public class WriteDbData4MyBatisMSSelectColumn implements BaseWriteDbData {
    private int recordId;
    private String mapperSimpleClassName;
    private String mapperMethodName;
    private String tableName;
    private String columnName;
    private String columnAlias;
    private String mapperClassName;
    private String xmlFileName;
    private String xmlFilePath;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getMapperSimpleClassName() {
        return mapperSimpleClassName;
    }

    public void setMapperSimpleClassName(String mapperSimpleClassName) {
        this.mapperSimpleClassName = mapperSimpleClassName;
    }

    public String getMapperMethodName() {
        return mapperMethodName;
    }

    public void setMapperMethodName(String mapperMethodName) {
        this.mapperMethodName = mapperMethodName;
    }

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public String getColumnName() {
        return columnName;
    }

    public void setColumnName(String columnName) {
        this.columnName = columnName;
    }

    public String getColumnAlias() {
        return columnAlias;
    }

    public void setColumnAlias(String columnAlias) {
        this.columnAlias = columnAlias;
    }

    public String getMapperClassName() {
        return mapperClassName;
    }

    public void setMapperClassName(String mapperClassName) {
        this.mapperClassName = mapperClassName;
    }

    public String getXmlFileName() {
        return xmlFileName;
    }

    public void setXmlFileName(String xmlFileName) {
        this.xmlFileName = xmlFileName;
    }

    public String getXmlFilePath() {
        return xmlFilePath;
    }

    public void setXmlFilePath(String xmlFilePath) {
        this.xmlFilePath = xmlFilePath;
    }

    @Override
    public String toString() {
        return "WriteDbData4MyBatisMSSelectColumn{" +
                "mapperSimpleClassName='" + mapperSimpleClassName + '\'' +
                ", mapperMethodName='" + mapperMethodName + '\'' +
                ", tableName='" + tableName + '\'' +
                ", columnName='" + columnName + '\'' +
                ", columnAlias='" + columnAlias + '\'' +
                ", mapperClassName='" + mapperClassName + '\'' +
                ", xmlFileName='" + xmlFileName + '\'' +
                ", xmlFilePath='" + xmlFilePath + '\'' +
                '}';
    }
}
