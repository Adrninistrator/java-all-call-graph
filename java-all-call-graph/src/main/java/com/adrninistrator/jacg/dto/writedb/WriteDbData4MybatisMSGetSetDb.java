package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/11/13
 * @description: 用于写入数据库的数据，使用MyBatis时get/set方法所关联的数据库信息（使用MySQL）
 */
public class WriteDbData4MybatisMSGetSetDb implements BaseWriteDbData {
    private int recordId;
    private int fldRelationshipId;
    private String getOrSet;
    private Integer getMethodCallId;
    private Integer setMethodCallId;
    private String dbOperate;
    private String tableName;
    private String columnName;
    private String columnRelateDesc;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public int getFldRelationshipId() {
        return fldRelationshipId;
    }

    public void setFldRelationshipId(int fldRelationshipId) {
        this.fldRelationshipId = fldRelationshipId;
    }

    public String getGetOrSet() {
        return getOrSet;
    }

    public void setGetOrSet(String getOrSet) {
        this.getOrSet = getOrSet;
    }

    public Integer getGetMethodCallId() {
        return getMethodCallId;
    }

    public void setGetMethodCallId(Integer getMethodCallId) {
        this.getMethodCallId = getMethodCallId;
    }

    public Integer getSetMethodCallId() {
        return setMethodCallId;
    }

    public void setSetMethodCallId(Integer setMethodCallId) {
        this.setMethodCallId = setMethodCallId;
    }

    public String getDbOperate() {
        return dbOperate;
    }

    public void setDbOperate(String dbOperate) {
        this.dbOperate = dbOperate;
    }

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

    public String getColumnRelateDesc() {
        return columnRelateDesc;
    }

    public void setColumnRelateDesc(String columnRelateDesc) {
        this.columnRelateDesc = columnRelateDesc;
    }
}
