package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，类的信息
 */
public class WriteDbData4ClassInfo extends AbstractWriteDbData {
    private final String simpleClassName;
    private final int accessFlags;
    private final String className;

    public WriteDbData4ClassInfo(String simpleClassName, int accessFlags, String className) {
        this.simpleClassName = simpleClassName;
        this.accessFlags = accessFlags;
        this.className = className;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public int getAccessFlags() {
        return accessFlags;
    }

    public String getClassName() {
        return className;
    }
}
