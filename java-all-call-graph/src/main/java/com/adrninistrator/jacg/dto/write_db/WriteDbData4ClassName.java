package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，引用的类
 */
public class WriteDbData4ClassName extends AbstractWriteDbData {
    private final String className;
    private final String simpleClassName;
    private final int duplicateClass;

    public WriteDbData4ClassName(String className, String simpleClassName, int duplicateClass) {
        this.className = className;
        this.simpleClassName = simpleClassName;
        this.duplicateClass = duplicateClass;
    }

    public String getClassName() {
        return className;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public int getDuplicateClass() {
        return duplicateClass;
    }
}
