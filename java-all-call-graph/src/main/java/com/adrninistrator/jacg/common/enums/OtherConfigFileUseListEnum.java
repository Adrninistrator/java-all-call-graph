package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.enums.interfaces.BaseConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseListEnum implements BaseConfigInterface {
    OCFULE_JAR_DIR(InputDirEnum.IDE_CONFIG.getDirName() + "/jar_dir.properties",
            "指定需要处理的jar包，或保存class、jar文件的目录"),
    OCFULE_FIND_STACK_KEYWORD_4EE(InputDirEnum.IDE_KEYWORD_CONF.getDirName() + "/find_stack_keyword_4ee.properties",
            "生成向上的方法完整调用链文件后，再查找到起始方法的调用堆栈时，使用的关键字"),
    OCFULE_FIND_STACK_KEYWORD_4ER(InputDirEnum.IDE_KEYWORD_CONF.getDirName() + "/find_stack_keyword_4er.properties",
            "生成向下的方法完整调用链文件后，再查找从起始方法开始的调用堆栈时，使用的关键字"),
    OCFULE_EXTENSIONS_CODE_PARSER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/code_parser.properties",
            "定义用于对代码进行解析的扩展类完整类名"),
    OCFULE_EXTENSIONS_EXTENDED_DATA_ADD(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/extended_data_add.properties",
            "定义根据方法调用信息添加方法调用自定义数据扩展类完整类名"),
    OCFULE_EXTENSIONS_EXTENDED_DATA_SUPPLEMENT(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/extended_data_supplement.properties",
            "定义对方法调用自定义数据进行补充的扩展类完整类名"),
    OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/method_annotation_formatter.properties",
            "定义处理方法上的注解生成用于显示信息的扩展类完整类名"),
    OCFULE_EXTENSIONS_METHOD_CALL_ADD(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/method_call_add.properties",
            "定义人工添加方法调用关系的扩展类完整类名"),
    OCFULE_EXTENSIONS_FIND_STACK_KEYWORD_FILTER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/find_stack_keyword_filter.properties",
            "定义用于对完整调用链文件生成调用堆栈时使用的过滤器扩展类完整类名"),
    ;

    private final String fileName;
    private final String desc;

    OtherConfigFileUseListEnum(String fileName, String desc) {
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
