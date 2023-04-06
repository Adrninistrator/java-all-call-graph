package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.enums.interfaces.MainConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum ConfigKeyEnum implements MainConfigInterface {
    CKE_APP_NAME("app.name", "当前应用的调用关系写入数据库里的表名后缀", String.class),
    CKE_CALL_GRAPH_OUTPUT_DETAIL("call.graph.output.detail", "生成调用链时的详细程度，1: 最详细，2: 中等，3: 最简单", String.class),
    CKE_THREAD_NUM("thread.num", "并发处理线程数量/数据源连接池数量", Integer.class),
    CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER("ignore.dup.callee.in.one.caller", "生成向下的调用链时，在一个调用方法中出现多次的被调用方法（包含方法调用业务功能数据），是否需要忽略", Boolean.class),
    CKE_OUTPUT_ROOT_PATH("output.root.path", "生成文件的根目录，以\"/\"或\"\\\\\"作为分隔符，末尾是否为分隔符不影响（默认为当前目录）", String.class),
    CKE_DB_INSERT_BATCH_SIZE("db.insert.batch.size", "批量写入数据库时每次插入的数量", Integer.class),
    CKE_CHECK_JAR_FILE_UPDATED("check.jar.file.updated", "检查jar包文件是否有更新", Boolean.class),
    ;

    // 参数key
    private final String key;
    // 参数描述
    private final String desc;
    // 参数类型
    private final Class<?> type;

    ConfigKeyEnum(String key, String desc, Class<?> type) {
        this.key = key;
        this.desc = desc;
        this.type = type;
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
    public Class<?> getType() {
        return type;
    }

    @Override
    public String toString() {
        return key;
    }

    @Override
    public String getFileName() {
        return InputDirEnum.IDE_CONFIG.getDirName() + "/config.properties";
    }

    public static String getDescFromKey(String key) {
        for (ConfigKeyEnum configKeyEnum : ConfigKeyEnum.values()) {
            if (configKeyEnum.getKey().equals(key)) {
                return configKeyEnum.getDesc();
            }
        }
        return "";
    }
}
