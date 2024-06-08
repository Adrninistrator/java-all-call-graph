package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/7/24
 * @description: 用于写入数据库的数据，MyBatis的Entity与Mapper、表名（使用MySQL）
 */
public class WriteDbData4MybatisMSEntity implements BaseWriteDbData {

    private String mapperSimpleClassName;
    private String entitySimpleClassName;
    private String tableName;
    private String mapperClassName;
    private String entityClassName;
    private String xmlFileName;
    private String xmlFilePath;

    public WriteDbData4MybatisMSEntity() {
    }

    public WriteDbData4MybatisMSEntity(String mapperSimpleClassName, String entitySimpleClassName, String tableName, String mapperClassName, String entityClassName,
                                       String xmlFileName, String xmlFilePath) {
        this.mapperSimpleClassName = mapperSimpleClassName;
        this.entitySimpleClassName = entitySimpleClassName;
        this.tableName = tableName;
        this.mapperClassName = mapperClassName;
        this.entityClassName = entityClassName;
        this.xmlFileName = xmlFileName;
        this.xmlFilePath = xmlFilePath;
    }

    public String getMapperSimpleClassName() {
        return mapperSimpleClassName;
    }

    public void setMapperSimpleClassName(String mapperSimpleClassName) {
        this.mapperSimpleClassName = mapperSimpleClassName;
    }

    public String getEntitySimpleClassName() {
        return entitySimpleClassName;
    }

    public void setEntitySimpleClassName(String entitySimpleClassName) {
        this.entitySimpleClassName = entitySimpleClassName;
    }

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }

    public String getMapperClassName() {
        return mapperClassName;
    }

    public void setMapperClassName(String mapperClassName) {
        this.mapperClassName = mapperClassName;
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
