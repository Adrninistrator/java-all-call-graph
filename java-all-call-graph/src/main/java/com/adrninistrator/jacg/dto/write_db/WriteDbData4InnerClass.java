package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description: 用于写入数据库的数据，内部类信息
 */
public class WriteDbData4InnerClass extends AbstractWriteDbData {
    public final String simpleInnerClassName;
    public final String innerClassName;
    public final String simpleOuterClassName;
    public final String outerClassName;
    public final int anonymousClass;

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

    public String getInnerClassName() {
        return innerClassName;
    }

    public String getSimpleOuterClassName() {
        return simpleOuterClassName;
    }

    public String getOuterClassName() {
        return outerClassName;
    }

    public int getAnonymousClass() {
        return anonymousClass;
    }
}

