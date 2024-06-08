package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，类的信息
 */
public class WriteDbData4ClassInfo implements BaseWriteDbData {
    private String simpleClassName;
    private int accessFlags;
    private String className;
    private String classFileHash;
    private int jarNum;

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

    public String getClassFileHash() {
        return classFileHash;
    }

    public void setClassFileHash(String classFileHash) {
        this.classFileHash = classFileHash;
    }

    public int getJarNum() {
        return jarNum;
    }

    public void setJarNum(int jarNum) {
        this.jarNum = jarNum;
    }

    @Override
    public String toString() {
        return className;
    }
}
