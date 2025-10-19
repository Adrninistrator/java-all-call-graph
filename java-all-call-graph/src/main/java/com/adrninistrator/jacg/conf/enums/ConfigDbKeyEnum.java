package com.adrninistrator.jacg.conf.enums;

import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;
import com.mysql.cj.jdbc.Driver;

/**
 * @author adrninistrator
 * @date 2022/11/14
 * @description:
 */
public enum ConfigDbKeyEnum implements MainConfigInterface {
    CDKE_DB_USE_H2("db.use.h2", new String[]{"是否使用H2数据库，true: 使用，false: 不使用"},
            Boolean.class, true, Boolean.TRUE.toString()),
    CDKE_DB_H2_FILE_PATH("db.h2.file.path", new String[]{"H2数据库文件路径（仅当使用H2数据库时需要指定），后缀“.mv.db”支持指定，也支持不指定",
            "需要使用绝对路径或相对路径。若指定为相对路径，则需要以 ./ 开头",
            "示例：D:/build/jacg_h2db.mv.db",
            "示例：./build/jacg_h2db.mv.db",
            "示例：D:/build/jacg_h2db",
            "示例：./build/jacg_h2db"},
            String.class, true, "./build/jacg_h2db"),
    CDKE_DB_DRIVER_NAME("db.driver.name", new String[]{"数据库配置（仅当使用非H2数据库时需要指定），驱动类名"},
            String.class, true, Driver.class.getName()),
    CDKE_DB_URL("db.url", new String[]{"数据库配置（仅当使用非H2数据库时需要指定），URL",
            "使用MySQL时，url需要指定rewriteBatchedStatements=true，开启批量插入，提高效率，默认未开启"},
            String.class, true, "jdbc:mysql://x.x.x.x:3306/database?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&rewriteBatchedStatements=true"),
    CDKE_DB_USERNAME("db.username", new String[]{"数据库配置（仅当使用非H2数据库时需要指定），用户名"},
            String.class, true, ""),
    CDKE_DB_PASSWORD("db.password", new String[]{"数据库配置（仅当使用非H2数据库时需要指定），密码"},
            String.class, true, ""),
    CDKE_DB_TABLE_SUFFIX("db.table.suffix", new String[]{"数据库表后缀，默认使用空不需要指定"},
            String.class, false, ""),
    CDKE_SLOW_QUERY_SWITCH("slow.query.switch", new String[]{"数据库慢查询监控开关，若开启，会在应用日志中打印慢查询相关信息，可搜索“出现慢查询”"},
            Boolean.class, false, Boolean.TRUE.toString()),
    CDKE_SLOW_QUERY_TIME("slow.query.time", new String[]{"数据库慢查询监控，时间阈值，单位为毫秒，查询耗时大于该值时记录慢查询日志"},
            Integer.class, false, "200"),
    CDKE_SLOW_QUERY_ROW_NUM("slow.query.row.num", new String[]{"数据库慢查询监控，查询结果数量阈值，查询结果数量大于该值时记录慢查询日志"},
            Integer.class, false, "5000"),
    ;

    // 参数key
    private final String key;
    // 参数描述
    private final String[] descriptions;
    // 参数类型
    private final Class<?> type;
    // 是否不允许为空
    private final boolean notBlank;
    // 默认值
    private final String defaultValue;

    ConfigDbKeyEnum(String key, String[] descriptions, Class<?> type, boolean notBlank, String defaultValue) {
        this.key = key;
        this.descriptions = descriptions;
        this.type = type;
        this.notBlank = notBlank;
        this.defaultValue = defaultValue;
    }

    @Override
    public String getEnumConstantName() {
        return name();
    }

    @Override
    public String getKey() {
        return key;
    }

    @Override
    public String[] getDescriptions() {
        return descriptions;
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
    public boolean isNotBlank() {
        return notBlank;
    }

    @Override
    public String getDefaultValue() {
        return defaultValue;
    }

    @Override
    public String toString() {
        return key;
    }

    @Override
    public String getFileName() {
        return InputDirEnum.IDE_CONFIG.getDirName() + "/config_db.properties";
    }
}
