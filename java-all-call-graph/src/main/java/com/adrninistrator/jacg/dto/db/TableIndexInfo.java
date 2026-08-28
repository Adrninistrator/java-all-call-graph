package com.adrninistrator.jacg.dto.db;

import com.adrninistrator.jacg.common.enums.IndexTypeEnum;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2026/6/27
 * @description: 数据库表索引信息
 */
public class TableIndexInfo {

    // 索引类型（主键索引、唯一索引、普通索引）
    private IndexTypeEnum indexType;

    // 索引名称
    private String indexName;

    // 索引对应的字段名列表（按索引定义顺序）
    private List<String> columnNames;

    public IndexTypeEnum getIndexType() {
        return indexType;
    }

    public void setIndexType(IndexTypeEnum indexType) {
        this.indexType = indexType;
    }

    public String getIndexName() {
        return indexName;
    }

    public void setIndexName(String indexName) {
        this.indexName = indexName;
    }

    public List<String> getColumnNames() {
        return columnNames;
    }

    public void setColumnNames(List<String> columnNames) {
        this.columnNames = columnNames;
    }

    @Override
    public String toString() {
        return "TableIndexInfo{" +
                "indexType=" + indexType +
                ", indexName='" + indexName + '\'' +
                ", columnNames=" + columnNames +
                '}';
    }
}
