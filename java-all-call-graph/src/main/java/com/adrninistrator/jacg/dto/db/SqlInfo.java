package com.adrninistrator.jacg.dto.db;

/**
 * @author adrninistrator
 * @date 2026/6/17
 * @description: SQL语句解析信息，包含被操作的表名及SQL语句类型
 */
public class SqlInfo {

    // 被操作的表名
    private String tableName;

    // SQL语句类型，取值为SqlTypeEnum中的值
    private String sqlType;

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public String getSqlType() {
        return sqlType;
    }

    public void setSqlType(String sqlType) {
        this.sqlType = sqlType;
    }

    @Override
    public String toString() {
        return "SqlInfo{" +
                "tableName='" + tableName + '\'' +
                ", sqlType='" + sqlType + '\'' +
                '}';
    }
}
