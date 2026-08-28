package com.adrninistrator.jacg.dto.db;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2026/6/27
 * @description: 数据库表结构化信息（字段信息 + 索引信息）
 */
public class TableStructureInfo {

    // 表名
    private String tableName;

    // 字段信息列表（按字段定义顺序）
    private List<TableColumnInfo> fieldList;

    // 索引信息列表
    private List<TableIndexInfo> indexList;

    // 建表语句
    private String createTableSql;

    // 数据库表额外描述，代表该表存储的数据及作用（用作代码图谱查询时给AI理解）
    private String extraDesc;

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public List<TableColumnInfo> getFieldList() {
        return fieldList;
    }

    public void setFieldList(List<TableColumnInfo> fieldList) {
        this.fieldList = fieldList;
    }

    public List<TableIndexInfo> getIndexList() {
        return indexList;
    }

    public void setIndexList(List<TableIndexInfo> indexList) {
        this.indexList = indexList;
    }

    public String getCreateTableSql() {
        return createTableSql;
    }

    public void setCreateTableSql(String createTableSql) {
        this.createTableSql = createTableSql;
    }

    public String getExtraDesc() {
        return extraDesc;
    }

    public void setExtraDesc(String extraDesc) {
        this.extraDesc = extraDesc;
    }

    @Override
    public String toString() {
        return "TableStructureInfo{" +
                "tableName='" + tableName + '\'' +
                ", fieldList=" + fieldList +
                ", indexList=" + indexList +
                ", createTableSql='" + createTableSql + '\'' +
                ", extraDesc='" + extraDesc + '\'' +
                '}';
    }
}
