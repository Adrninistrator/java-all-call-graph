package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/3/14
 * @description: 用于写入数据库的数据，MyBatis Mapper方法操作的数据库表信息（使用MySQL）
 */
public class WriteDbData4MyBatisMSTable implements BaseWriteDbData {
    private String mapperSimpleClassName;
    private String mapperMethodName;
    private String sqlStatement;
    private int tableSeq;
    private String tableName;
    private String mapperClassName;
    private String xmlFileName;
    private String xmlFilePath;

    public WriteDbData4MyBatisMSTable() {
    }

    public WriteDbData4MyBatisMSTable(String mapperSimpleClassName, String mapperMethodName, String sqlStatement, int tableSeq, String tableName, String mapperClassName,
                                      String xmlFileName, String xmlFilePath) {
        this.mapperSimpleClassName = mapperSimpleClassName;
        this.mapperMethodName = mapperMethodName;
        this.sqlStatement = sqlStatement;
        this.tableSeq = tableSeq;
        this.tableName = tableName;
        this.mapperClassName = mapperClassName;
        this.xmlFileName = xmlFileName;
        this.xmlFilePath = xmlFilePath;
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

    public String getSqlStatement() {
        return sqlStatement;
    }

    public void setSqlStatement(String sqlStatement) {
        this.sqlStatement = sqlStatement;
    }

    public int getTableSeq() {
        return tableSeq;
    }

    public void setTableSeq(int tableSeq) {
        this.tableSeq = tableSeq;
    }

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
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
