package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2026/6/19
 * @description: Spring依赖注入字段信息
 */
public class WriteDbData4SpringDIField implements BaseWriteDbData {

    // 记录ID
    private int recordId;

    // 完整类名
    private String className;

    // 唯一类名
    private String simpleClassName;

    // 字段声明类型（接口类型）
    private String fieldType;

    // 唯一字段声明类型
    private String simpleFieldType;

    // 字段名
    private String fieldName;

    // Spring Bean实际类型
    private String beanType;

    // 唯一Bean类型
    private String simpleBeanType;

    // 字段声明类型与Spring Bean实际注入类型是否相同，0:不同，1:相同
    private int sameType;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public String getSimpleFieldType() {
        return simpleFieldType;
    }

    public void setSimpleFieldType(String simpleFieldType) {
        this.simpleFieldType = simpleFieldType;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getBeanType() {
        return beanType;
    }

    public void setBeanType(String beanType) {
        this.beanType = beanType;
    }

    public String getSimpleBeanType() {
        return simpleBeanType;
    }

    public void setSimpleBeanType(String simpleBeanType) {
        this.simpleBeanType = simpleBeanType;
    }

    public int getSameType() {
        return sameType;
    }

    public void setSameType(int sameType) {
        this.sameType = sameType;
    }
}
