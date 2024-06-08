package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

import java.util.Date;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 用于写入数据库的数据，jar包信息
 */
public class WriteDbData4JarInfo implements BaseWriteDbData {
    private int jarNum;
    private String jarType;
    private String jarPathHash;
    private String jarFullPath;
    private String jarFileName;
    private String jarFileNameHead;
    private String jarFileNameExt;
    private String lastModifiedTime;
    private String jarFileHash;
    private Date importTime;

    public int getJarNum() {
        return jarNum;
    }

    public void setJarNum(int jarNum) {
        this.jarNum = jarNum;
    }

    public String getJarType() {
        return jarType;
    }

    public void setJarType(String jarType) {
        this.jarType = jarType;
    }

    public String getJarPathHash() {
        return jarPathHash;
    }

    public void setJarPathHash(String jarPathHash) {
        this.jarPathHash = jarPathHash;
    }

    public String getJarFullPath() {
        return jarFullPath;
    }

    public void setJarFullPath(String jarFullPath) {
        this.jarFullPath = jarFullPath;
    }

    public String getJarFileName() {
        return jarFileName;
    }

    public void setJarFileName(String jarFileName) {
        this.jarFileName = jarFileName;
    }

    public String getJarFileNameHead() {
        return jarFileNameHead;
    }

    public void setJarFileNameHead(String jarFileNameHead) {
        this.jarFileNameHead = jarFileNameHead;
    }

    public String getJarFileNameExt() {
        return jarFileNameExt;
    }

    public void setJarFileNameExt(String jarFileNameExt) {
        this.jarFileNameExt = jarFileNameExt;
    }

    public String getLastModifiedTime() {
        return lastModifiedTime;
    }

    public void setLastModifiedTime(String lastModifiedTime) {
        this.lastModifiedTime = lastModifiedTime;
    }

    public String getJarFileHash() {
        return jarFileHash;
    }

    public void setJarFileHash(String jarFileHash) {
        this.jarFileHash = jarFileHash;
    }

    public Date getImportTime() {
        return importTime;
    }

    public void setImportTime(Date importTime) {
        this.importTime = importTime;
    }
}
