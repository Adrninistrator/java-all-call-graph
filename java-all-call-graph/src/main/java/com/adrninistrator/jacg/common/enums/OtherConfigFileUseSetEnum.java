package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.enums.interfaces.BaseConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseSetEnum implements BaseConfigInterface {
    OCFUSE_ALLOWED_CLASS_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/allowed_class_prefix.properties",
            "将java-callgraph2生成的直接调用关系文件写入数据库时使用的配置，需要处理的类名前缀"),
    OCFUSE_METHOD_CLASS_4CALLEE(InputDirEnum.IDE_CONFIG.getDirName() + "/method_class_4callee.properties",
            "生成调用指定类/方法的所有向上的方法完整调用链时的配置文件,指定需要生成的类名"),
    OCFUSE_METHOD_CLASS_4CALLER(InputDirEnum.IDE_CONFIG.getDirName() + "/method_class_4caller.properties",
            "生成指定类/方法调用的所有向下的方法完整调用链时的配置文件，指定需要生成的类名，与方法前缀，以及起始代码行号、结束代码行号"),
    OCFUSE_IGNORE_CLASS_KEYWORD(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_class_keyword.properties",
            "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的类名关键字"),
    OCFUSE_IGNORE_FULL_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_full_method_prefix.properties",
            "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的完整方法前缀"),
    OCFUSE_IGNORE_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_method_prefix.properties",
            "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的方法名前缀"),
    ;

    private final String fileName;
    private final String desc;

    OtherConfigFileUseSetEnum(String fileName, String desc) {
        this.fileName = fileName;
        this.desc = desc;
    }

    @Override
    public String getKey() {
        return fileName;
    }

    @Override
    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return fileName;
    }
}
