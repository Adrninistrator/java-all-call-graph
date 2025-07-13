package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/6/16
 * @description: 用于写入数据库的数据，Spring的包扫描路径
 */
public class WriteDbData4SpringScanPackage implements BaseWriteDbData {

    private int recordId;
    private String type;
    private int seq;
    private String scanPackage;
    private String defineClassNameXmlPath;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getScanPackage() {
        return scanPackage;
    }

    public void setScanPackage(String scanPackage) {
        this.scanPackage = scanPackage;
    }

    public String getDefineClassNameXmlPath() {
        return defineClassNameXmlPath;
    }

    public void setDefineClassNameXmlPath(String defineClassNameXmlPath) {
        this.defineClassNameXmlPath = defineClassNameXmlPath;
    }
}
