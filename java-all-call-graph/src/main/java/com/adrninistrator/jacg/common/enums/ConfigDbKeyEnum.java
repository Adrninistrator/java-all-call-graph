package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.enums.interfaces.BaseConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/11/14
 * @description:
 */
public enum ConfigDbKeyEnum implements BaseConfigInterface {
    CDKE_DB_USE_H2("db.use.h2", "是否使用H2数据库"),
    CDKE_DB_H2_FILE_PATH("db.h2.file.path", "H2数据库文件路径（仅当使用H2数据库时需要指定）"),
    CDKE_DB_DRIVER_NAME("db.driver.name", "数据库配置（仅当使用非H2数据库时需要指定），驱动类名"),
    CDKE_DB_URL("db.url", "数据库配置（仅当使用非H2数据库时需要指定），URL"),
    CDKE_DB_USERNAME("db.username", "数据库配置（仅当使用非H2数据库时需要指定），用户名"),
    CDKE_DB_PASSWORD("db.password", "数据库配置（仅当使用非H2数据库时需要指定），密码"),
    ;

    private final String key;
    private final String desc;

    ConfigDbKeyEnum(String key, String desc) {
        this.key = key;
        this.desc = desc;
    }

    @Override
    public String getKey() {
        return key;
    }

    @Override
    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return key;
    }
}
