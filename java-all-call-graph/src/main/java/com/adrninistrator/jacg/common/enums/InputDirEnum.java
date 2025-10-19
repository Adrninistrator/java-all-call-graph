package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/8/21
 * @description:
 */
public enum InputDirEnum {
    IDE_CONFIG("_jacg_config", "基本的配置文件目录"),
    IDE_SQL("_jacg_sql", "保存sql脚本的目录"),
    IDE_KEYWORD_CONF("_jacg_find_stack_keyword", "保存需要查找的关键字的目录"),
    IDE_EXTENSIONS("_jacg_extensions", "保存扩展类配置文件的目录"),
    IDE_BUSINESS_DATA_TYPE("_jacg_business_data_type", "保存业务功能数据类型配置文件的目录"),
    IDE_JAR_DIFF("_jacg_jar_diff", "比较新旧两个目录中不同版本jar文件相关配置文件的目录"),
    IDE_COMPATIBILITY("_jacg_jar_compatibility", "Jar兼容性检查相关配置文件的目录"),
    IDE_GEN_ALL_CALL_GRAPH("_jacg_gen_all_call_graph", "生成完整方法调用链相关开关配置文件所在目录"),
    IDE_SPRING_AOP("_jacg_spring_aop", "处理Spring AOP相关开关配置文件所在目录"),
    IDE_EL_EXAMPLE("_el_example", "el表达式配置说明文件所在目录"),
    ;

    private final String dirName;
    private final String desc;

    InputDirEnum(String dirName, String desc) {
        this.dirName = dirName;
        this.desc = desc;
    }

    public String getDirName() {
        return dirName;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return dirName;
    }
}
