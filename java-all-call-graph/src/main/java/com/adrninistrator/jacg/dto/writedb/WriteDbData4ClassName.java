package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，引用的类
 */
public class WriteDbData4ClassName implements BaseWriteDbData {
    private String className;
    private String simpleClassName;
    private int duplicateClass;

    public WriteDbData4ClassName() {
    }

    public WriteDbData4ClassName(String className, String simpleClassName, int duplicateClass) {
        this.className = className;
        this.simpleClassName = simpleClassName;
        this.duplicateClass = duplicateClass;
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

    public int getDuplicateClass() {
        return duplicateClass;
    }

    public void setDuplicateClass(int duplicateClass) {
        this.duplicateClass = duplicateClass;
    }
}
