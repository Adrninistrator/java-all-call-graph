package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/10/8
 * @description: 用于写入数据库的数据，MyBatis的XML中update set子句的字段信息（使用MySQL）
 */
public class WriteDbData4MyBatisMSSetColumn implements BaseWriteDbData {
    private String mapperSimpleClassName;
    private String mapperMethodName;
    private String tableName;
    private String columnName;
    private String paramObjName;
    private String paramName;
    private String paramRawName;
    private String mapperClassName;
    private String xmlFileName;
    private String xmlFilePath;

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

    public String getParamObjName() {
        return paramObjName;
    }

    public void setParamObjName(String paramObjName) {
        this.paramObjName = paramObjName;
    }

    public String getParamName() {
        return paramName;
    }

    public void setParamName(String paramName) {
        this.paramName = paramName;
    }

    public String getParamRawName() {
        return paramRawName;
    }

    public void setParamRawName(String paramRawName) {
        this.paramRawName = paramRawName;
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
}
