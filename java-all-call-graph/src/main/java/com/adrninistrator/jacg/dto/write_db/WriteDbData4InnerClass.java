package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description: 用于写入数据库的数据，内部类信息
 */
public class WriteDbData4InnerClass extends AbstractWriteDbData {
    public String simpleInnerClassName;
    public String innerClassName;
    public String simpleOuterClassName;
    public String outerClassName;
    public int anonymousClass;

    public WriteDbData4InnerClass() {
    }

    public WriteDbData4InnerClass(String simpleInnerClassName, String innerClassName, String simpleOuterClassName, String outerClassName, int anonymousClass) {
        this.simpleInnerClassName = simpleInnerClassName;
        this.innerClassName = innerClassName;
        this.simpleOuterClassName = simpleOuterClassName;
        this.outerClassName = outerClassName;
        this.anonymousClass = anonymousClass;
    }

    public String getSimpleInnerClassName() {
        return simpleInnerClassName;
    }

    public void setSimpleInnerClassName(String simpleInnerClassName) {
        this.simpleInnerClassName = simpleInnerClassName;
    }

    public String getInnerClassName() {
        return innerClassName;
    }

    public void setInnerClassName(String innerClassName) {
        this.innerClassName = innerClassName;
    }

    public String getSimpleOuterClassName() {
        return simpleOuterClassName;
    }

    public void setSimpleOuterClassName(String simpleOuterClassName) {
        this.simpleOuterClassName = simpleOuterClassName;
    }

    public String getOuterClassName() {
        return outerClassName;
    }

    public void setOuterClassName(String outerClassName) {
        this.outerClassName = outerClassName;
    }

    public int getAnonymousClass() {
        return anonymousClass;
    }

    public void setAnonymousClass(int anonymousClass) {
        this.anonymousClass = anonymousClass;
    }
}

