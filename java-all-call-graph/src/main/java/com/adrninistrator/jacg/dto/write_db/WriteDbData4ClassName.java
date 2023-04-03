package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，引用的类
 */
public class WriteDbData4ClassName extends AbstractWriteDbData {
    private final String className;
    private final String simpleClassName;

    public WriteDbData4ClassName(String className, String simpleClassName) {
        this.className = className;
        this.simpleClassName = simpleClassName;
    }

    public String getClassName() {
        return className;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }
}
