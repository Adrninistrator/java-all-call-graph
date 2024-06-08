package com.adrninistrator.jacg.unpacker.common.enums;

/**
 * @author adrninistrator
 * @date 2024/1/22
 * @description: .jar文件所在位置枚举
 */
public enum JarFileLocationEnum {
    JFLE_TAR_JAR("@TAR_JAR@", ".tar.gz中的.jar"),
    JFLE_TAR_WAR("@TAR_WAR@", ".tar.gz中的.war"),
    JFLE_TAR_WAR_JAR("@TAR_WAR_JAR@", ".tar.gz中的.war中的.jar"),
    JFLE_TAR_JAR_JAR("@TAR_JAR_JAR@", ".tar.gz中的.jar中的.jar"),
    ;

    private final String type;
    private final String desc;

    JarFileLocationEnum(String type, String desc) {
        this.type = type;
        this.desc = desc;
    }

    public String getType() {
        return type;
    }

    public String getDesc() {
        return desc;
    }
}
