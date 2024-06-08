package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/7/19
 * @description: 用于写入数据库的数据，通过get/set方法关联的字段关系表
 */
public class WriteDbData4FieldRelationship implements BaseWriteDbData {

    private int fldRelationshipId;
    private int getMethodCallId;
    private int setMethodCallId;
    private String callerFullMethod;
    private int callerLineNumber;
    private String getSimpleClassName;
    private String getMethodName;
    private String getClassName;
    private String setSimpleClassName;
    private String setMethodName;
    private String setClassName;
    private int valid;
    private String type;
    private int relationshipFlags;
    private int beanUtilCallId;
    private String beanUtilMethod;

    public int getFldRelationshipId() {
        return fldRelationshipId;
    }

    public void setFldRelationshipId(int fldRelationshipId) {
        this.fldRelationshipId = fldRelationshipId;
    }

    public int getGetMethodCallId() {
        return getMethodCallId;
    }

    public void setGetMethodCallId(int getMethodCallId) {
        this.getMethodCallId = getMethodCallId;
    }

    public int getSetMethodCallId() {
        return setMethodCallId;
    }

    public void setSetMethodCallId(int setMethodCallId) {
        this.setMethodCallId = setMethodCallId;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public void setCallerFullMethod(String callerFullMethod) {
        this.callerFullMethod = callerFullMethod;
    }

    public int getCallerLineNumber() {
        return callerLineNumber;
    }

    public void setCallerLineNumber(int callerLineNumber) {
        this.callerLineNumber = callerLineNumber;
    }

    public String getGetSimpleClassName() {
        return getSimpleClassName;
    }

    public void setGetSimpleClassName(String getSimpleClassName) {
        this.getSimpleClassName = getSimpleClassName;
    }

    public String getGetMethodName() {
        return getMethodName;
    }

    public void setGetMethodName(String getMethodName) {
        this.getMethodName = getMethodName;
    }

    public String getGetClassName() {
        return getClassName;
    }

    public void setGetClassName(String getClassName) {
        this.getClassName = getClassName;
    }

    public String getSetSimpleClassName() {
        return setSimpleClassName;
    }

    public void setSetSimpleClassName(String setSimpleClassName) {
        this.setSimpleClassName = setSimpleClassName;
    }

    public String getSetMethodName() {
        return setMethodName;
    }

    public void setSetMethodName(String setMethodName) {
        this.setMethodName = setMethodName;
    }

    public String getSetClassName() {
        return setClassName;
    }

    public void setSetClassName(String setClassName) {
        this.setClassName = setClassName;
    }

    public int getValid() {
        return valid;
    }

    public void setValid(int valid) {
        this.valid = valid;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getRelationshipFlags() {
        return relationshipFlags;
    }

    public void setRelationshipFlags(int relationshipFlags) {
        this.relationshipFlags = relationshipFlags;
    }

    public int getBeanUtilCallId() {
        return beanUtilCallId;
    }

    public void setBeanUtilCallId(int beanUtilCallId) {
        this.beanUtilCallId = beanUtilCallId;
    }

    public String getBeanUtilMethod() {
        return beanUtilMethod;
    }

    public void setBeanUtilMethod(String beanUtilMethod) {
        this.beanUtilMethod = beanUtilMethod;
    }

    @Override
    public String toString() {
        return "get='" + getClassName + '\'' +
                getMethodName + '\'' +
                ", set='" + setClassName + '\'' +
                setMethodName + '\'';
    }
}
