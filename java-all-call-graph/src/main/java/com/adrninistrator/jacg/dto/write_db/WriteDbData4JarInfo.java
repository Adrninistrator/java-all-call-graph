package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 用于写入数据库的数据，jar包信息
 */
public class WriteDbData4JarInfo extends AbstractWriteDbData {
    private int jarNum;
    private String jarType;
    private String jarPathHash;
    private String jarFullPath;
    private String jarFileName;
    private String lastModified;
    private String jarHash;

    public WriteDbData4JarInfo() {
    }

    public WriteDbData4JarInfo(int jarNum, String jarType, String jarPathHash, String jarFullPath, String jarFileName, String lastModified, String jarHash) {
        this.jarNum = jarNum;
        this.jarType = jarType;
        this.jarPathHash = jarPathHash;
        this.jarFullPath = jarFullPath;
        this.jarFileName = jarFileName;
        this.lastModified = lastModified;
        this.jarHash = jarHash;
    }

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

    public String getLastModified() {
        return lastModified;
    }

    public void setLastModified(String lastModified) {
        this.lastModified = lastModified;
    }

    public String getJarHash() {
        return jarHash;
    }

    public void setJarHash(String jarHash) {
        this.jarHash = jarHash;
    }
}
