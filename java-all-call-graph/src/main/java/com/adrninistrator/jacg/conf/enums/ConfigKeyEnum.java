package com.adrninistrator.jacg.conf.enums;

import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum ConfigKeyEnum implements MainConfigInterface {
    CKE_APP_NAME("app.name",
            new String[]{"当前应用的调用关系写入数据库里的表名后缀，分隔符不能使用-，需要使用_"},
            String.class, true, "test"),
    CKE_THREAD_NUM("thread.num",
            new String[]{"并发处理线程数量/数据源连接池数量（若超过了需要处理的任务数量，会使用任务数量作为线程数量）"},
            Integer.class, true, "20"),
    CKE_DB_INSERT_BATCH_SIZE("db.insert.batch.size",
            new String[]{"批量写入数据库时每次插入的数量"},
            Integer.class, true, "1000"),
    CKE_DROP_OR_TRUNCATE_TABLE("drop.or.truncate.table",
            new String[]{"在插入数据库表前，对表执行 DROP(false) 还是 TRUNCATE(true) 操作"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_OUTPUT_ROOT_PATH("output.root.path",
            new String[]{"生成调用链文件的根目录路径，以\"/\"或\"\\\\\"作为分隔符，末尾是否为分隔符不影响（默认为当前目录）"},
            String.class, false, ""),
    CKE_OUTPUT_DIR_FLAG("output.dir.flag",
            new String[]{"生成调用链文件的目录名中的标志，完整目录名使用{app.name}{output.dir.flag}_{当前时间}，默认为空"},
            String.class, false, ""),
    CKE_OUTPUT_DIR_NAME("output.dir.name",
            new String[]{"生成调用链文件的目录名，非空时目录名使用当前值，为空时使用上一个参数说明的格式"},
            String.class, false, ""),
    CKE_CHECK_JAR_FILE_UPDATED("check.jar.file.updated",
            new String[]{"生成调用链文件时，是否检查jar包文件有更新，若发现jar包文件内容发生变化则不生成，false:不检查，true:检查"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_CALL_GRAPH_OUTPUT_DETAIL("call.graph.output.detail",
            new String[]{"生成调用链时的详细程度",
                    "0: 最详细 完整类名+方法名+方法参数+返回类型",
                    "1: 详细 完整类名+方法名+方法参数",
                    "2: 中等 完整类名+方法名",
                    "3: 最简单 简单类名（对于同名类展示完整类名）+方法名"},
            String.class, true, OutputDetailEnum.ODE_1.getDetail()),
    CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER("ignore.dup.callee.in.one.caller",
            new String[]{"生成向下的调用链时，在一个调用方法中出现多次的被调用方法（包含方法调用业务功能数据），是否需要忽略，true:忽略，false:不忽略"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_CALL_GRAPH_GEN_SEPARATE_STACK("call.graph.gen.separate.stack",
            new String[]{"生成方法调用链时，是否需要为每个调用堆栈生成独立的文件，仅当 call.graph.output.detail=1 时支持"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_CALL_GRAPH_GEN_JSON_CALLER("call.graph.gen.json.caller",
            new String[]{"生成向下的方法调用链时，是否需要输出JSON格式的内容"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_GEN_CALL_GRAPH_NUM_LIMIT("gen.call.graph.num.limit",
            new String[]{"生成调用链文件时，每个方法允许生成的方法调用数量限制，默认为0，小于等于0代表不限制"},
            Integer.class, false, "0"),
    CKE_CALL_GRAPH_WRITE_TO_FILE("call.graph.write.to.file",
            new String[]{"生成调用链时，是否需要将调用链数据写入文件"},
            Boolean.class, false, Boolean.TRUE.toString()),
    CKE_CALL_GRAPH_RETURN_IN_MEMORY("call.graph.return.in.memory",
            new String[]{"生成调用链时，是否需要在内存中返回调用链数据",
                    "不能与 " + CKE_CALL_GRAPH_WRITE_TO_FILE.getKey() + " 开关同时设置为false",
                    OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER.getKey() + " 或 " + OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE.getKey() + " 配置文件中只能一个需要处理的方法",
                    "设置为 true 时，可通过 " + CKE_GEN_CALL_GRAPH_NUM_LIMIT.getKey() + " 开关设置允许生成的方法调用数量限制"},
            Boolean.class, false, Boolean.FALSE.toString()),
    CKE_CALL_GRAPH_FILE_SHORT_MODE("call.graph.file.short.mode",
            new String[]{"生成调用链时，文件名是否使用更短的模式，以避免超过Windows文件系统支持的长度",
                    "若是，则文件名仅包含对应方法的HASH+长度；若否，则文件名还会包含方法的唯一类名及方法名"},
            Boolean.class, false, Boolean.FALSE.toString()),
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

    ConfigKeyEnum(String key, String[] descriptions, Class<?> type, boolean notBlank, String defaultValue) {
        this.key = key;
        this.descriptions = descriptions;
        this.type = type;
        this.notBlank = notBlank;
        this.defaultValue = defaultValue;
    }

    @Override
    public String getEnumName() {
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
        return key + " " + ConfigKeyEnum.class.getSimpleName() + "." + name();
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
    public String getDefaultValue() {
        return defaultValue;
    }

    @Override
    public String toString() {
        return key;
    }

    @Override
    public String getFileName() {
        return InputDirEnum.IDE_CONFIG.getDirName() + "/config.properties";
    }
}
