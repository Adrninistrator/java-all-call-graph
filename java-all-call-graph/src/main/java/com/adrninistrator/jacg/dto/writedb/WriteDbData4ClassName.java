package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，类名
 */
public class WriteDbData4ClassName implements BaseWriteDbData {
    private int recordId;
    private String className;
    private String simpleClassName;
    private int duplicateClass;

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

    public int getDuplicateClass() {
        return duplicateClass;
    }

    public void setDuplicateClass(int duplicateClass) {
        this.duplicateClass = duplicateClass;
    }
}
