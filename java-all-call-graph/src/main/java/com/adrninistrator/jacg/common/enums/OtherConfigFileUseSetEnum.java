package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.JACGConstants;

import java.io.File;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseSetEnum {
    OCFUSE_IN_ALLOWED_CLASS_PREFIX(JACGConstants.DIR_CONFIG + File.separator + "i_allowed_class_prefix.properties",
            "将java-callgraph2生成的直接调用关系文件写入数据库时使用的配置，需要处理的类名前缀"),
    OCFUSE_OUT_GRAPH_FOR_CALLEE_CLASS_NAME(JACGConstants.DIR_CONFIG + File.separator + "o_g4callee_class_name.properties",
            "生成调用指定类的所有向上的方法完整调用链时的配置文件,指定需要生成的类名"),
    OCFUSE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD(JACGConstants.DIR_CONFIG + File.separator + "o_g4caller_entry_method.properties",
            "生成指定类调用的所有向下的方法完整调用链时的配置文件，指定需要生成的类名，与方法名前缀，以及起始代码行号、结束代码行号"),
    OCFUSE_OUT_GRAPH_FOR_CALLER_ENTRY_METHOD_IGNORE_PREFIX(JACGConstants.DIR_CONFIG + File.separator + "o_g4caller_entry_method_ignore_prefix.properties",
            "生成指定类调用的所有向下的方法完整调用链时的配置文件，在该文件中指定需要忽略的方法前缀"),
    OCFUSE_OUT_GRAPH_FOR_CALLER_IGNORE_CLASS_KEYWORD(JACGConstants.DIR_CONFIG + File.separator + "o_g4caller_ignore_class_keyword.properties",
            "生成指定类调用的所有向下的方法完整调用链时的配置文件，指定忽略的类名关键字"),
    OCFUSE_OUT_GRAPH_FOR_CALLER_IGNORE_FULL_METHOD_PREFIX(JACGConstants.DIR_CONFIG + File.separator + "o_g4caller_ignore_full_method_prefix.properties",
            "生成指定类调用的所有向下的方法完整调用链时的配置文件，指定忽略的完整方法前缀"),
    OCFUSE_OUT_GRAPH_FOR_CALLER_IGNORE_METHOD_PREFIX(JACGConstants.DIR_CONFIG + File.separator + "o_g4caller_ignore_method_prefix.properties",
            "生成指定类调用的所有向下的方法完整调用链时的配置文件，指定忽略的方法名前缀"),
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
}
