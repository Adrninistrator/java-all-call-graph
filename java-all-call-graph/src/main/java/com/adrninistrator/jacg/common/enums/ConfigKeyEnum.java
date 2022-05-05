package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum ConfigKeyEnum {
    CKE_APPNAME("app.name", "当前应用的调用关系写入数据库里的表名后缀"),
    CKE_CALL_GRAPH_JAR_LIST("call.graph.jar.list", "需要通过java-callgraph2生成调用关系文件的jar包，或保存class、jar文件的目录列表"),
    CKE_INPUT_IGNORE_OTHER_PACKAGE("input.ignore.other.package", "将调用关系及类名写入数据库中时，是否忽略非指定包中的类"),
    CKE_CALL_GRAPH_OUTPUT_DETAIL("call.graph.output.detail", "生成调用链时的详细程度"),
    CKE_THREAD_NUM("thread.num", "并发处理线程数量/数据源连接池数量"),
    CKE_SHOW_METHOD_ANNOTATION("show.method.annotation", "生成调用链时，是否显示方法注解"),
    CKE_GEN_COMBINED_OUTPUT("gen.combined.output", "生成调用链时，是否需要生成合并的文件"),
    CKE_SHOW_CALLER_LINE_NUM("show.caller.line.num", "生成调用链时，是否需要显示调用者源代码行号"),
    CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER("ignore.dup.callee.in.one.caller", "生成向下的调用链时，在一个调用方法中出现多次的被调用方法（包含自定义数据），是否需要忽略"),
    CKE_DB_USE_H2("db.use.h2", "是否使用H2数据库"),
    CKE_DB_H2_FILE_PATH("db.h2.file.path", "H2数据库文件路径（仅当使用H2数据库时需要指定）"),
    CKE_DB_DRIVER_NAME("db.driver.name", "数据库配置（仅当使用非H2数据库时需要指定），驱动类名"),
    CKE_DB_URL("db.url", "数据库配置（仅当使用非H2数据库时需要指定），URL"),
    CKE_DB_USERNAME("db.username", "数据库配置（仅当使用非H2数据库时需要指定），用户名"),
    CKE_DB_PASSWORD("db.password", "数据库配置（仅当使用非H2数据库时需要指定），密码"),
    ;

    private String key;
    private String desc;

    ConfigKeyEnum(String key, String desc) {
        this.key = key;
        this.desc = desc;
    }

    public String getKey() {
        return key;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return key;
    }
}
