package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/3/14
 * @description: 用于写入数据库的数据，MyBatis数据库表信息（使用MySQL）
 */
public class WriteDbData4MyBatisMSTable extends AbstractWriteDbData {
    private final String mapperSimpleClassName;
    private final String mapperMethodName;
    private final String sqlStatement;
    private final int tableSeq;
    private final String tableName;
    private final String mapperClassName;

    public WriteDbData4MyBatisMSTable(String mapperSimpleClassName, String mapperMethodName, String sqlStatement, int tableSeq, String tableName, String mapperClassName) {
        this.mapperSimpleClassName = mapperSimpleClassName;
        this.mapperMethodName = mapperMethodName;
        this.sqlStatement = sqlStatement;
        this.tableSeq = tableSeq;
        this.tableName = tableName;
        this.mapperClassName = mapperClassName;
    }

    public String getMapperSimpleClassName() {
        return mapperSimpleClassName;
    }

    public String getMapperMethodName() {
        return mapperMethodName;
    }

    public String getSqlStatement() {
        return sqlStatement;
    }

    public int getTableSeq() {
        return tableSeq;
    }

    public String getTableName() {
        return tableName;
    }

    public String getMapperClassName() {
        return mapperClassName;
    }
}
