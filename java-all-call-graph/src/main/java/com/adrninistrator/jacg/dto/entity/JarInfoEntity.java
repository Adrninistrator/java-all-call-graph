package com.adrninistrator.jacg.dto.entity;

/**
 * @author adrninistrator
 * @date 2022/2/8
 * @description:
 */
public class JarInfoEntity {
    // jar包文件路径
    private final String jarFilePath;

    // jar包类型
    private final String jarType;

    public JarInfoEntity(String jarFilePath, String jarType) {
        this.jarFilePath = jarFilePath;
        this.jarType = jarType;
    }

    public String getJarFilePath() {
        return jarFilePath;
    }

    public String getJarType() {
        return jarType;
    }
}
