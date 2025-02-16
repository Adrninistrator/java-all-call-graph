package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 用于写入数据库的数据，类的信息
 */
public class WriteDbData4ClassInfo implements BaseWriteDbData {
    private int recordId;
    private String simpleClassName;
    private int accessFlags;
    private String className;
    private String packageName;
    private int packageLevel;
    private String classFileHash;
    private int jarNum;
    private String classPathInJar;

    @Override
    public String toString() {
        return className;
    }

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
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

    public String getPackageName() {
        return packageName;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public int getPackageLevel() {
        return packageLevel;
    }

    public void setPackageLevel(int packageLevel) {
        this.packageLevel = packageLevel;
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

    public String getClassPathInJar() {
        return classPathInJar;
    }

    public void setClassPathInJar(String classPathInJar) {
        this.classPathInJar = classPathInJar;
    }
}
