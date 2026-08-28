package com.adrninistrator.jacg.dto.db;

/**
 * @author adrninistrator
 * @date 2026/6/17
 * @description: 数据库表字段信息
 */
public class TableColumnInfo {

    // 表名
    private String tableName;

    // 字段名称
    private String columnName;

    // 字段类型
    private String columnType;

    // 字段描述
    private String columnComment;

    // 字段精度
    private int precision;

    // 字段小数位（非数值类型为0）
    private int scale;

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

    public String getColumnType() {
        return columnType;
    }

    public void setColumnType(String columnType) {
        this.columnType = columnType;
    }

    public String getColumnComment() {
        return columnComment;
    }

    public void setColumnComment(String columnComment) {
        this.columnComment = columnComment;
    }

    public int getPrecision() {
        return precision;
    }

    public void setPrecision(int precision) {
        this.precision = precision;
    }

    public int getScale() {
        return scale;
    }

    public void setScale(int scale) {
        this.scale = scale;
    }

    @Override
    public String toString() {
        return "TableColumnInfo{" +
                "tableName='" + tableName + '\'' +
                ", columnName='" + columnName + '\'' +
                ", columnType='" + columnType + '\'' +
                ", columnComment='" + columnComment + '\'' +
                ", precision=" + precision +
                ", scale=" + scale +
                '}';
    }
}
