package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2024/8/17
 * @description: 用于写入数据库的数据，引用的类
 */
public class WriteDbData4ClassReference implements BaseWriteDbData {

    private int recordId;
    private String className;
    private String simpleClassName;
    private String referencedClassName;
    private String referencedSimpleClassName;
    private int jarNum;

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

    public String getReferencedClassName() {
        return referencedClassName;
    }

    public void setReferencedClassName(String referencedClassName) {
        this.referencedClassName = referencedClassName;
    }

    public String getReferencedSimpleClassName() {
        return referencedSimpleClassName;
    }

    public void setReferencedSimpleClassName(String referencedSimpleClassName) {
        this.referencedSimpleClassName = referencedSimpleClassName;
    }

    public int getJarNum() {
        return jarNum;
    }

    public void setJarNum(int jarNum) {
        this.jarNum = jarNum;
    }
}

