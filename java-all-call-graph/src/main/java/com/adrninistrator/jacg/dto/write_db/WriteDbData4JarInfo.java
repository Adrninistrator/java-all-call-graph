package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 用于写入数据库的数据，jar包信息
 */
public class WriteDbData4JarInfo extends AbstractWriteDbData {
    private final int jarNum;
    private final String jarType;
    private final String jarPathHash;
    private final String jarFullPath;
    private final String lastModified;
    private final String jarHash;

    public WriteDbData4JarInfo(int jarNum, String jarType, String jarPathHash, String jarFullPath, String lastModified, String jarHash) {
        this.jarNum = jarNum;
        this.jarType = jarType;
        this.jarPathHash = jarPathHash;
        this.jarFullPath = jarFullPath;
        this.lastModified = lastModified;
        this.jarHash = jarHash;
    }

    public int getJarNum() {
        return jarNum;
    }

    public String getJarType() {
        return jarType;
    }

    public String getJarPathHash() {
        return jarPathHash;
    }

    public String getJarFullPath() {
        return jarFullPath;
    }

    public String getLastModified() {
        return lastModified;
    }

    public String getJarHash() {
        return jarHash;
    }
}
