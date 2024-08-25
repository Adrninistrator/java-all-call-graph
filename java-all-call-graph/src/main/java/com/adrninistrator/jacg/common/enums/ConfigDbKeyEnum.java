package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.enums.interfaces.MainConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/11/14
 * @description:
 */
public enum ConfigDbKeyEnum implements MainConfigInterface {
    CDKE_DB_USE_H2("db.use.h2", "是否使用H2数据库", Boolean.class, true),
    CDKE_DB_H2_FILE_PATH("db.h2.file.path", "H2数据库文件路径（仅当使用H2数据库时需要指定）", String.class, true),
    CDKE_DB_DRIVER_NAME("db.driver.name", "数据库配置（仅当使用非H2数据库时需要指定），驱动类名", String.class, true),
    CDKE_DB_URL("db.url", "数据库配置（仅当使用非H2数据库时需要指定），URL", String.class, true),
    CDKE_DB_USERNAME("db.username", "数据库配置（仅当使用非H2数据库时需要指定），用户名", String.class, true),
    CDKE_DB_PASSWORD("db.password", "数据库配置（仅当使用非H2数据库时需要指定），密码", String.class, true),
    CDKE_DB_TABLE_SUFFIX("db.table.suffix", "数据库表后缀", String.class, false),
    ;

    // 参数key
    private final String key;
    // 参数描述
    private final String desc;
    // 参数类型
    private final Class<?> type;
    // 是否不允许为空
    private final boolean notBlank;

    ConfigDbKeyEnum(String key, String desc, Class<?> type, boolean notBlank) {
        this.key = key;
        this.desc = desc;
        this.type = type;
        this.notBlank = notBlank;
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
    public String getConfigPrintInfo() {
        return key + " " + ConfigDbKeyEnum.class.getSimpleName() + "." + name();
    }

    @Override
    public Class<?> getType() {
        return type;
    }

    @Override
    public boolean notBlank() {
        return notBlank;
    }

    @Override
    public String toString() {
        return key;
    }

    @Override
    public String getFileName() {
        return InputDirEnum.IDE_CONFIG.getDirName() + "/config_db.properties";
    }

    public static String getDescFromKey(String key) {
        for (ConfigDbKeyEnum configDbKeyEnum : ConfigDbKeyEnum.values()) {
            if (configDbKeyEnum.getKey().equals(key)) {
                return configDbKeyEnum.getDesc();
            }
        }
        return "";
    }
}
