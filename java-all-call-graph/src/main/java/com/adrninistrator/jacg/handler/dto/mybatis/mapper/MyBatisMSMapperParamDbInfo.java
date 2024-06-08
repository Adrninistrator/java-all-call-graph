package com.adrninistrator.jacg.handler.dto.mybatis.mapper;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2023/11/7
 * @description: MyBatis Mapper方法参数在sql语句中对应的数据库信息（支持MySQL）
 */
public class MyBatisMSMapperParamDbInfo {

    // 数据库表名
    private String tableName;

    // 数据库字段名
    private String columnName;

    // Mapper方法参数中的对象名称
    private String paramObjName;

    // Mapper方法参数中的参数名称
    private String paramName;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MyBatisMSMapperParamDbInfo that = (MyBatisMSMapperParamDbInfo) o;
        return tableName.equals(that.tableName) && columnName.equals(that.columnName) && paramObjName.equals(that.paramObjName) && paramName.equals(that.paramName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(tableName, columnName, paramObjName, paramName);
    }

    @Override
    public String toString() {
        return "MyBatisMSMapperParamDbInfo{" +
                "tableName='" + tableName + '\'' +
                ", columnName='" + columnName + '\'' +
                ", paramObjName='" + paramObjName + '\'' +
                ", paramName='" + paramName + '\'' +
                '}';
    }
}
