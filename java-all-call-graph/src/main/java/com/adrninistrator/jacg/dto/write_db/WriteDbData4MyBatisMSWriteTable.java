package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/3/14
 * @description: 用于写入数据库的数据，MyBatis写数据库表信息（使用MySQL）
 */
public class WriteDbData4MyBatisMSWriteTable extends AbstractWriteDbData {
    private final int recordId;
    private final String mapperSimpleClassName;
    private final String mapperMethodName;
    private final String sqlStatement;
    private final String tableName;
    private final String mapperClassName;

    public WriteDbData4MyBatisMSWriteTable(int recordId, String mapperSimpleClassName, String mapperMethodName, String sqlStatement, String tableName, String mapperClassName) {
        this.recordId = recordId;
        this.mapperSimpleClassName = mapperSimpleClassName;
        this.mapperMethodName = mapperMethodName;
        this.sqlStatement = sqlStatement;
        this.tableName = tableName;
        this.mapperClassName = mapperClassName;
    }

    public int getRecordId() {
        return recordId;
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

    public String getTableName() {
        return tableName;
    }

    public String getMapperClassName() {
        return mapperClassName;
    }
}
