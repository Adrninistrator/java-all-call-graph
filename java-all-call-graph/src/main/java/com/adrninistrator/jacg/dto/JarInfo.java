package com.adrninistrator.jacg.dto;

/**
 * @author adrninistrator
 * @date 2022/2/8
 * @description:
 */
public class JarInfo {

    // jar包文件路径
    private String jarFilePath;

    // jar包类型
    private String jarType;

    public String getJarFilePath() {
        return jarFilePath;
    }

    public void setJarFilePath(String jarFilePath) {
        this.jarFilePath = jarFilePath;
    }

    public String getJarType() {
        return jarType;
    }

    public void setJarType(String jarType) {
        this.jarType = jarType;
    }
}
