package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/3/14
 * @description: 用于写入数据库的数据，MyBatis写数据库表信息（使用MySQL）
 */
public class WriteDbData4MyBatisMSWriteTable extends AbstractWriteDbData {
    private String mapperSimpleClassName;
    private String mapperMethodName;
    private String sqlStatement;
    private String tableName;
    private String mapperClassName;

    public WriteDbData4MyBatisMSWriteTable() {
    }

    public WriteDbData4MyBatisMSWriteTable(String mapperSimpleClassName, String mapperMethodName, String sqlStatement, String tableName, String mapperClassName) {
        this.mapperSimpleClassName = mapperSimpleClassName;
        this.mapperMethodName = mapperMethodName;
        this.sqlStatement = sqlStatement;
        this.tableName = tableName;
        this.mapperClassName = mapperClassName;
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
}
