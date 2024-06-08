package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/7/24
 * @description: 用于写入数据库的数据，MyBatis的Entity与数据库字段名信息（使用MySQL）
 */
public class WriteDbData4MybatisMSColumn implements BaseWriteDbData {

    private String entitySimpleClassName;
    private String entityFieldName;
    private String columnName;
    private String entityClassName;
    private String xmlFileName;
    private String xmlFilePath;

    public WriteDbData4MybatisMSColumn() {
    }

    public WriteDbData4MybatisMSColumn(String entitySimpleClassName, String entityFieldName, String columnName, String entityClassName, String xmlFileName,
                                       String xmlFilePath) {
        this.entitySimpleClassName = entitySimpleClassName;
        this.entityFieldName = entityFieldName;
        this.columnName = columnName;
        this.entityClassName = entityClassName;
        this.xmlFileName = xmlFileName;
        this.xmlFilePath = xmlFilePath;
    }

    public String getEntitySimpleClassName() {
        return entitySimpleClassName;
    }

    public void setEntitySimpleClassName(String entitySimpleClassName) {
        this.entitySimpleClassName = entitySimpleClassName;
    }

    public String getEntityFieldName() {
        return entityFieldName;
    }

    public void setEntityFieldName(String entityFieldName) {
        this.entityFieldName = entityFieldName;
    }

    public String getColumnName() {
        return columnName;
    }

    public void setColumnName(String columnName) {
        this.columnName = columnName;
    }

    public String getEntityClassName() {
        return entityClassName;
    }

    public void setEntityClassName(String entityClassName) {
        this.entityClassName = entityClassName;
    }

    public String getXmlFileName() {
        return xmlFileName;
    }

    public void setXmlFileName(String xmlFileName) {
        this.xmlFileName = xmlFileName;
    }

    public String getXmlFilePath() {
        return xmlFilePath;
    }

    public void setXmlFilePath(String xmlFilePath) {
        this.xmlFilePath = xmlFilePath;
    }
}
