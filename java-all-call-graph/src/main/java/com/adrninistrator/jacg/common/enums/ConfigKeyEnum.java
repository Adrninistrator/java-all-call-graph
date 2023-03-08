package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.enums.interfaces.BaseConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum ConfigKeyEnum implements BaseConfigInterface {
    CKE_APP_NAME("app.name", "当前应用的调用关系写入数据库里的表名后缀"),
    CKE_CALL_GRAPH_OUTPUT_DETAIL("call.graph.output.detail", "生成调用链时的详细程度，1: 最详细，2: 中等，3: 最简单"),
    CKE_THREAD_NUM("thread.num", "并发处理线程数量/数据源连接池数量"),
    CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER("ignore.dup.callee.in.one.caller", "生成向下的调用链时，在一个调用方法中出现多次的被调用方法（包含方法调用自定义数据），是否需要忽略"),
    CKE_OUTPUT_ROOT_PATH("output.root.path", "生成文件的根目录，以\"/\"或\"\\\\\"作为分隔符，末尾是否为分隔符不影响，默认为当前目录"),
    CKE_DB_INSERT_BATCH_SIZE("db.insert.batch.size", "批量写入数据库时每次插入的数量"),
    CKE_CHECK_JAR_FILE_UPDATED("check.jar.file.updated", "检查jar包文件是否有更新"),
    CKE_CALLER_SHOW_RAW_METHOD_CALL_INFO("caller.show.raw.method.call.info", "生成向下的方法完整调用链时，是否显示原始方法调用信息"),
    ;

    private final String key;
    private final String desc;

    ConfigKeyEnum(String key, String desc) {
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
