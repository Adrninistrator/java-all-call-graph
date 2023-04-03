package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.enums.interfaces.ConfigInterface;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseSetEnum implements ConfigInterface {
    OCFUSE_ALLOWED_CLASS_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/allowed_class_prefix.properties",
            "将java-callgraph2生成的方法调用关系文件写入数据库时使用的配置，需要处理的类名前缀"),
    OCFUSE_METHOD_CLASS_4CALLEE(InputDirEnum.IDE_CONFIG.getDirName() + "/method_class_4callee.properties",
            "生成调用指定类/方法的所有向上的方法完整调用链时的配置文件,指定需要生成的类名"),
    OCFUSE_METHOD_CLASS_4CALLER(InputDirEnum.IDE_CONFIG.getDirName() + "/method_class_4caller.properties",
            "生成指定类/方法调用的所有向下的方法完整调用链时的配置文件，指定需要生成的类名，与方法前缀，以及起始代码行号、结束代码行号"),
    OCFUSE_IGNORE_CLASS_KEYWORD(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_class_keyword.properties",
            "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的类名关键字，可指定包名中的关键字，或类名中的关键字"),
    OCFUSE_IGNORE_FULL_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_full_method_prefix.properties",
            "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的完整方法前缀，可指定包名，或包名+类名，或包名+类名+方法名，或包名+类名+方法名+参数"),
    OCFUSE_IGNORE_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_method_prefix.properties",
            "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的方法名前缀"),
    OCFULE_BUSINESS_DATA_TYPE_SHOW_4EE(InputDirEnum.IDE_BUSINESS_DATA_TYPE.getDirName() + "/business_data_type_show_4ee.properties",
            "生成向上的完整方法调用链时，需要显示的业务功能数据类型。若不指定则不显示业务功能数据"),
    OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER(InputDirEnum.IDE_BUSINESS_DATA_TYPE.getDirName() + "/business_data_type_show_4er.properties",
            "生成向下的完整方法调用链时，需要显示的业务功能数据类型。若不指定则不显示业务功能数据"),
    ;

    // 参数配置文件名
    private final String fileName;
    // 参数配置描述
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

    public static String getDescFromKey(String key) {
        for (OtherConfigFileUseSetEnum otherConfigFileUseSetEnum : OtherConfigFileUseSetEnum.values()) {
            if (otherConfigFileUseSetEnum.getKey().equals(key)) {
                return otherConfigFileUseSetEnum.getDesc();
            }
        }
        return "";
    }
}
