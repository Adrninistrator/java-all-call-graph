package com.adrninistrator.jacg.common.enums;

import com.adrninistrator.jacg.common.enums.interfaces.ConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseSetEnum implements ConfigInterface {
    OCFUSE_ALLOWED_CLASS_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/allowed_class_prefix.properties",
            false, "将java-callgraph2生成的方法调用关系文件写入数据库时使用的配置，需要处理的类名前缀"),
    OCFUSE_METHOD_CLASS_4CALLEE(InputDirEnum.IDE_CONFIG.getDirName() + "/method_class_4callee.properties",
            false, "生成调用指定类/方法的所有向上的方法完整调用链时的配置文件,指定需要生成的类名，或类名+方法前缀/代码行号"),
    OCFUSE_METHOD_CLASS_4CALLER(InputDirEnum.IDE_CONFIG.getDirName() + "/method_class_4caller.properties",
            false, "生成指定类/方法调用的所有向下的方法完整调用链时的配置文件，指定需要生成的类名+方法前缀/代码行号，可指定起始代码行号、结束代码行号"),
    OCFUSE_IGNORE_CALL_TYPE(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_call_type.properties",
            false, "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的方法调用类型，指定 JavaCG2CallTypeEnum 枚举中的type"),
    OCFUSE_IGNORE_METHOD_TYPE_4CALLER(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_method_type_4caller.properties",
            false, "生成指定类/方法调用的所有向下的方法完整调用链时的配置文件，指定忽略的方法类型，指定 JACGMethodTypeEnum 枚举中的type"),
    OCFUSE_IGNORE_CLASS_KEYWORD(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_class_keyword.properties",
            false, "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的类名关键字，可指定包名中的关键字，或类名中的关键字"),
    OCFUSE_IGNORE_FULL_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_full_method_prefix.properties",
            false, "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的完整方法前缀，可指定包名，或包名+类名，或包名+类名+方法名，或包名+类名+方法名+参数"),
    OCFUSE_IGNORE_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_method_prefix.properties",
            false, "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定忽略的方法名前缀"),
    OCFUSE_INCLUDE_FULL_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/include_full_method_prefix.properties",
            false, "生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件，指定需要包含的完整方法前缀，优先级低于 allowed_class_prefix.properties、ignore_call_type.properties，优先级高于 ignore_class_keyword" +
            ".properties、ignore_full_method_prefix.properties、ignore_method_prefix.properties，可指定包名，或包名+类名，或包名+类名+方法名，或包名+类名+方法名+参数"),
    OCFULE_BUSINESS_DATA_TYPE_SHOW_4EE(InputDirEnum.IDE_BUSINESS_DATA_TYPE.getDirName() + "/business_data_type_show_4ee.properties",
            false, "生成向上的完整方法调用链时，需要显示的业务功能数据类型。若不指定则不显示业务功能数据"),
    OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER(InputDirEnum.IDE_BUSINESS_DATA_TYPE.getDirName() + "/business_data_type_show_4er.properties",
            false, "生成向下的完整方法调用链时，需要显示的业务功能数据类型。若不指定则不显示业务功能数据"),
    OCFULE_FR_EQ_CONVERSION_METHOD(InputDirEnum.IDE_FIELD_RELATIONSHIP.getDirName() + "/fr_eq_conversion_method.properties",
            true, "在处理通过get/set方法的字段关联关系时使用，指定方法返回值与被调用对象或参数属于等值转换的方法，在java-callgraph2中使用"),
    ;

    // 参数配置文件名
    private final String fileName;
    // 当代码中未配置当前参数时，是否使用配置文件中的参数
    private final boolean canUseConfigInFile;
    // 参数配置描述
    private final String desc;

    OtherConfigFileUseSetEnum(String fileName, boolean canUseConfigInFile, String desc) {
        this.fileName = fileName;
        this.canUseConfigInFile = canUseConfigInFile;
        this.desc = desc;
    }

    @Override
    public String getEnumName() {
        return name();
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
    public String getConfigPrintInfo() {
        return fileName + " " + OtherConfigFileUseSetEnum.class.getSimpleName() + "." + name();
    }

    public boolean isCanUseConfigInFile() {
        return canUseConfigInFile;
    }

    public static OtherConfigFileUseSetEnum getFromKey(String key) {
        for (OtherConfigFileUseSetEnum otherConfigFileUseSetEnum : OtherConfigFileUseSetEnum.values()) {
            if (otherConfigFileUseSetEnum.getKey().equals(key)) {
                return otherConfigFileUseSetEnum;
            }
        }
        throw new JavaCG2RuntimeException("不存在的key " + key);
    }

    @Override
    public String toString() {
        return fileName;
    }
}
