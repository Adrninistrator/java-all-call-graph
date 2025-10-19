package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/10/13
 * @description: 用于写入数据库的数据，包名信息
 */
public class WriteDbData4PackageInfo implements BaseWriteDbData {

    private int recordId;
    private String packageName;
    private int packageLevel;
    private int jarNum;
    private String jarFileName;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
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

    public int getJarNum() {
        return jarNum;
    }

    public void setJarNum(int jarNum) {
        this.jarNum = jarNum;
    }

    public String getJarFileName() {
        return jarFileName;
    }

    public void setJarFileName(String jarFileName) {
        this.jarFileName = jarFileName;
    }
}
