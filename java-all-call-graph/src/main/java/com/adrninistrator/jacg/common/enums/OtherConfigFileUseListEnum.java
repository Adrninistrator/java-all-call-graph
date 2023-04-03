package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.enums.interfaces.ConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseListEnum implements ConfigInterface {
    OCFULE_JAR_DIR(InputDirEnum.IDE_CONFIG.getDirName() + "/jar_dir.properties",
            "指定需要处理的jar包，或保存class、jar文件的目录"),
    OCFULE_FIND_STACK_KEYWORD_4EE(InputDirEnum.IDE_KEYWORD_CONF.getDirName() + "/find_stack_keyword_4ee.properties",
            "生成向上的方法完整调用链文件后，再查找到起始方法的调用堆栈时，使用的关键字"),
    OCFULE_FIND_STACK_KEYWORD_4ER(InputDirEnum.IDE_KEYWORD_CONF.getDirName() + "/find_stack_keyword_4er.properties",
            "生成向下的方法完整调用链文件后，再查找从起始方法开始的调用堆栈时，使用的关键字"),
    OCFULE_EXTENSIONS_CODE_PARSER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/code_parser.properties",
            "定义用于对代码进行解析的扩展类完整类名"),
    OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/method_annotation_formatter.properties",
            "定义处理方法上的注解生成用于显示信息的扩展类完整类名"),
    OCFULE_EXTENSIONS_MANUAL_ADD_METHOD_CALL1(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/manual_add_method_call1.properties",
            "定义人工添加方法调用关系的扩展类完整类名，处理特定的子类与实现类"),
    OCFULE_EXTENSIONS_FIND_STACK_KEYWORD_FILTER(InputDirEnum.IDE_EXTENSIONS.getDirName() + "/find_stack_keyword_filter.properties",
            "定义用于对完整调用链文件生成调用堆栈时使用的过滤器扩展类完整类名"),
    ;

    // 参数配置文件名
    private final String fileName;
    // 参数配置描述
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

    public static String getDescFromKey(String key) {
        for (OtherConfigFileUseListEnum otherConfigFileUseListEnum : OtherConfigFileUseListEnum.values()) {
            if (otherConfigFileUseListEnum.getKey().equals(key)) {
                return otherConfigFileUseListEnum.getDesc();
            }
        }
        return "";
    }
}
