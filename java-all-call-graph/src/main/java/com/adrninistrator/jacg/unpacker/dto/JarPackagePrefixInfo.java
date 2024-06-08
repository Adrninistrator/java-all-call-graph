package com.adrninistrator.jacg.unpacker.dto;

import com.adrninistrator.jacg.unpacker.common.enums.JarFileLocationEnum;

/**
 * @author adrninistrator
 * @date 2024/1/22
 * @description: .jar中的包名前缀信息
 */
public class JarPackagePrefixInfo {

    // .tar.gz文件名称
    private String tarFileName;

    // 当前的.jar文件所在位置枚举
    private JarFileLocationEnum jarFileLocationEnum;

    // .tar.gz中的.war或外层.jar文件名称，包含路径
    private String warOrOuterJarFileName = "";

    // .tar.gz中的.jar，或.tar.gz中的.war中的.jar，或.tar.gz中的.jar中的.jar文件名称，包含路径
    private String jarFileName = "";

    // .tar.gz中的.war或外层.jar文件名称，仅包含文件名称，不包含路径
    private String warOrOuterJarFileOnlyName = "";

    // .tar.gz中的.jar，或.tar.gz中的.war中的.jar，或.tar.gz中的.jar中的.jar文件名称，仅包含文件名称，不包含路径
    private String jarFileOnlyName = "";

    // .war或.jar中的包名前缀
    private String packagePrefix;

    //
    public String getTarFileName() {
        return tarFileName;
    }

    public void setTarFileName(String tarFileName) {
        this.tarFileName = tarFileName;
    }

    public JarFileLocationEnum getJarFileLocationEnum() {
        return jarFileLocationEnum;
    }

    public void setJarFileLocationEnum(JarFileLocationEnum jarFileLocationEnum) {
        this.jarFileLocationEnum = jarFileLocationEnum;
    }

    public String getWarOrOuterJarFileName() {
        return warOrOuterJarFileName;
    }

    public void setWarOrOuterJarFileName(String warOrOuterJarFileName) {
        this.warOrOuterJarFileName = warOrOuterJarFileName;
    }

    public String getJarFileName() {
        return jarFileName;
    }

    public void setJarFileName(String jarFileName) {
        this.jarFileName = jarFileName;
    }

    public String getWarOrOuterJarFileOnlyName() {
        return warOrOuterJarFileOnlyName;
    }

    public void setWarOrOuterJarFileOnlyName(String warOrOuterJarFileOnlyName) {
        this.warOrOuterJarFileOnlyName = warOrOuterJarFileOnlyName;
    }

    public String getJarFileOnlyName() {
        return jarFileOnlyName;
    }

    public void setJarFileOnlyName(String jarFileOnlyName) {
        this.jarFileOnlyName = jarFileOnlyName;
    }

    public String getPackagePrefix() {
        return packagePrefix;
    }

    public void setPackagePrefix(String packagePrefix) {
        this.packagePrefix = packagePrefix;
    }
}
