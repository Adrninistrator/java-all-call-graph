package com.adrninistrator.jacg.dto.writedb.base;

import org.neo4j.ogm.annotation.Transient;

/**
 * @author adrninistrator
 * @date 2023/7/25
 * @description: 用于写入数据库的数据的父类，get/set方法
 */
public class BaseWriteDbData4GetSetMethod implements BaseWriteDbData {

    private int recordId;
    private String simpleClassName;
    private String methodName;
    private String fieldName;
    private String fieldCategory;
    private String simpleFieldTypeNad;
    private String fieldTypeNad;
    private int arrayDimensions;
    private String className;
    private String methodHash;
    private String fullMethod;
    private String returnType;

    // 不出现在数据库表中的字段
    // 当前方法是否在超类中
    @Transient
    private boolean inSuperClass;

    // 当前属于get方法还是set方法
    @Transient
    private boolean getOrSet;

    // 当前get/set方法对应的被调用时的方法调用id
    @Transient
    private int methodCallId;
    // 不出现在数据库表中的字段

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getFieldCategory() {
        return fieldCategory;
    }

    public void setFieldCategory(String fieldCategory) {
        this.fieldCategory = fieldCategory;
    }

    public String getSimpleFieldTypeNad() {
        return simpleFieldTypeNad;
    }

    public void setSimpleFieldTypeNad(String simpleFieldTypeNad) {
        this.simpleFieldTypeNad = simpleFieldTypeNad;
    }

    public String getFieldTypeNad() {
        return fieldTypeNad;
    }

    public void setFieldTypeNad(String fieldTypeNad) {
        this.fieldTypeNad = fieldTypeNad;
    }

    public int getArrayDimensions() {
        return arrayDimensions;
    }

    public void setArrayDimensions(int arrayDimensions) {
        this.arrayDimensions = arrayDimensions;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    public boolean isInSuperClass() {
        return inSuperClass;
    }

    public void setInSuperClass(boolean inSuperClass) {
        this.inSuperClass = inSuperClass;
    }

    public boolean isGetOrSet() {
        return getOrSet;
    }

    public void setGetOrSet(boolean getOrSet) {
        this.getOrSet = getOrSet;
    }

    public int getMethodCallId() {
        return methodCallId;
    }

    public void setMethodCallId(int methodCallId) {
        this.methodCallId = methodCallId;
    }

    @Override
    public String toString() {
        return "BaseWriteDbData4GetSetMethod{" +
                "recordId=" + recordId +
                ", simpleClassName='" + simpleClassName + '\'' +
                ", methodName='" + methodName + '\'' +
                ", fieldName='" + fieldName + '\'' +
                ", fieldCategory='" + fieldCategory + '\'' +
                ", simpleFieldType='" + simpleFieldTypeNad + '\'' +
                ", fieldType='" + fieldTypeNad + '\'' +
                ", className='" + className + '\'' +
                ", methodHash='" + methodHash + '\'' +
                ", fullMethod='" + fullMethod + '\'' +
                ", returnType='" + returnType + '\'' +
                '}';
    }
}
