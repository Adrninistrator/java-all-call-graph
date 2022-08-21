package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseSetEnum {
    OCFUSE_IN_ALLOWED_CLASS_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/i_allowed_class_prefix.properties",
            "将java-callgraph2生成的直接调用关系文件写入数据库时使用的配置，需要处理的类名前缀"),
    OCFUSE_OUT_GRAPH_FOR_CALLEE_CLASS_NAME(InputDirEnum.IDE_CONFIG.getDirName() + "/o_g4callee_class_name.properties",
            "生成调用指定类的所有向上的方法完整调用链时的配置文件,指定需要生成的类名"),
    OCFUSE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD(InputDirEnum.IDE_CONFIG.getDirName() + "/o_g4caller_entry_method.properties",
            "生成指定类调用的所有向下的方法完整调用链时的配置文件，指定需要生成的类名，与方法前缀，以及起始代码行号、结束代码行号"),
    OCFUSE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD_IGNORE_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/o_g4caller_entry_method_ignore_prefix.properties",
            "生成指定类调用的所有向下的方法完整调用链时的配置文件，在该文件中指定需要忽略的方法前缀"),
    OCFUSE_OUT_GRAPH_FOR_CALLER_IGNORE_CLASS_KEYWORD(InputDirEnum.IDE_CONFIG.getDirName() + "/o_g4caller_ignore_class_keyword.properties",
            "生成指定类调用的所有向下的方法完整调用链时的配置文件，指定忽略的类名关键字"),
    OCFUSE_OUT_GRAPH_FOR_CALLER_IGNORE_FULL_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/o_g4caller_ignore_full_method_prefix.properties",
            "生成指定类调用的所有向下的方法完整调用链时的配置文件，指定忽略的完整方法前缀"),
    OCFUSE_OUT_GRAPH_FOR_CALLER_IGNORE_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/o_g4caller_ignore_method_prefix.properties",
            "生成指定类调用的所有向下的方法完整调用链时的配置文件，指定忽略的方法名前缀"),
    OCFUSE_EXTENSIONS_CODE_PARSER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/code_parser.properties",
            "定义用于对代码进行解析的自定义处理类的完整类名"),
    OCFUSE_EXTENSIONS_EXTENDED_DATA_ADD(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/extended_data_add.properties",
            "定义用于添加自定义数据处理类的完整类名"),
    OCFUSE_EXTENSIONS_EXTENDED_DATA_SUPPLEMENT(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/extended_data_supplement.properties",
            "定义用于对自定义数据进行补充的自定义处理类的完整类名"),
    OCFUSE_EXTENSIONS_METHOD_ANNOTATION_HANDLER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/method_annotation_handler.properties",
            "定义用于对方法上的注解进行处理的类完整类名"),
    ;

    private String fileName;
    private String desc;

    OtherConfigFileUseSetEnum(String fileName, String desc) {
        this.fileName = fileName;
        this.desc = desc;
    }

    public String getFileName() {
        return fileName;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return fileName;
    }
}
