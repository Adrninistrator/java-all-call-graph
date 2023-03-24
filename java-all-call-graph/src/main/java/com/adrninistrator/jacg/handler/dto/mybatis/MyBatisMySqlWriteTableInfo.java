package com.adrninistrator.jacg.handler.dto.mybatis;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author adrninistrator
 * @date 2023/3/16
 * @description: MyBatis中写的数据库表信息（支持MySQL）
 */
public class MyBatisMySqlWriteTableInfo {
    // sql语句类型
    @JsonProperty("statement")
    private final String sqlStatement;

    // sql语句类型
    @JsonProperty("table")
    private final String tableName;

    public MyBatisMySqlWriteTableInfo(String sqlStatement, String tableName) {
        this.sqlStatement = sqlStatement;
        this.tableName = tableName;
    }

    public String getSqlStatement() {
        return sqlStatement;
    }

    public String getTableName() {
        return tableName;
    }
}
