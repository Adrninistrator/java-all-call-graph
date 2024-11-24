package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description: 用于写入数据库的数据，内部类信息
 */
public class WriteDbData4InnerClass implements BaseWriteDbData {
    private String innerSimpleClassName;
    private String innerClassName;
    private String outerSimpleClassName;
    private String outerClassName;
    private int anonymousClass;

    public WriteDbData4InnerClass() {
    }

    public WriteDbData4InnerClass(String innerSimpleClassName, String innerClassName, String outerSimpleClassName, String outerClassName, int anonymousClass) {
        this.innerSimpleClassName = innerSimpleClassName;
        this.innerClassName = innerClassName;
        this.outerSimpleClassName = outerSimpleClassName;
        this.outerClassName = outerClassName;
        this.anonymousClass = anonymousClass;
    }

    public String getInnerSimpleClassName() {
        return innerSimpleClassName;
    }

    public void setInnerSimpleClassName(String innerSimpleClassName) {
        this.innerSimpleClassName = innerSimpleClassName;
    }

    public String getInnerClassName() {
        return innerClassName;
    }

    public void setInnerClassName(String innerClassName) {
        this.innerClassName = innerClassName;
    }

    public String getOuterSimpleClassName() {
        return outerSimpleClassName;
    }

    public void setOuterSimpleClassName(String outerSimpleClassName) {
        this.outerSimpleClassName = outerSimpleClassName;
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

