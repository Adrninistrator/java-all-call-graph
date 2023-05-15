package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，类的信息
 */
public class WriteDbData4ClassInfo extends AbstractWriteDbData {
    private String simpleClassName;
    private int accessFlags;
    private String className;

    public WriteDbData4ClassInfo() {
    }

    public WriteDbData4ClassInfo(String simpleClassName, int accessFlags, String className) {
        this.simpleClassName = simpleClassName;
        this.accessFlags = accessFlags;
        this.className = className;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public int getAccessFlags() {
        return accessFlags;
    }

    public void setAccessFlags(int accessFlags) {
        this.accessFlags = accessFlags;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }
}
