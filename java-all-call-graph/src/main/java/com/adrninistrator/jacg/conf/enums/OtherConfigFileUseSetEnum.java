package com.adrninistrator.jacg.conf.enums;

import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseSetEnum implements OtherConfigInterface {
    OCFUSE_METHOD_CLASS_4CALLEE(InputDirEnum.IDE_CONFIG.getDirName() + "/method_class_4callee.properties",
            new String[]{"(作用) 生成调用指定类的所有向上的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）",
                    "(内容) 指定需要生成的类名，或类名+方法前缀/代码行号",
                    "(格式1) {类名}",
                    "(格式2) {类名}:{方法名}",
                    "(格式3) {类名}:{方法名}({参数})",
                    "(格式4) {类名}:{代码行号}",
                    "(格式5) {类名}:{方法名}({参数}):{方法返回类型}",
                    "(格式说明) 假如只指定了类名，没有指定方法名或代码行号，则处理指定类的全部方法",
                    "(格式说明) 假如指定了方法名或代码行号，则处理指定类的对应方法",
                    "(格式说明) {类名}可指定简单类名或完整类名；若存在同名类，则需要指定完整类名",
                    "(格式说明) {代码行号}可指定某个方法对应的任意代码行号，如C:f1()方法代码起止行号范围为{100,203}，则可指定以上范围的任意数字代表需要处理C:f1()方法",
                    "(格式说明) {方法返回类型}需要指定完整类型",
                    "(格式说明) 假如某个方法是接口中未实现的方法或抽象方法，则不支持指定代码行号的方式，需要指定方法前缀",
                    "(示例)",
                    "Test1",
                    "com.test.Test1",
                    "Test1:test",
                    "Test1:test(",
                    "Test1:test(java.lang.String)",
                    "Test1:234",
                    "Test1:test(java.lang.String):java.lang.String"}
            , null),
    OCFUSE_METHOD_CLASS_4CALLER(InputDirEnum.IDE_CONFIG.getDirName() + "/method_class_4caller.properties",
            new String[]{"(作用) 生成指定类调用的所有向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）",
                    "(内容) 指定需要生成的类名+方法前缀/代码行号，可指定起始代码行号、结束代码行号",
                    "(格式1) {类名}",
                    "(格式2) {类名}:{方法名} {起始代码行号}-{结束代码行号}",
                    "(格式3) {类名}:{方法名}({参数}) {起始代码行号}-{结束代码行号}",
                    "(格式3) {类名}:{方法名}({参数}):{方法返回类型} {起始代码行号}-{结束代码行号}",
                    "(格式4) {类名}:{代码行号} {起始代码行号}-{结束代码行号}",
                    "(格式说明) 假如仅指定了{类名}，则会处理对应类的所有方法",
                    "(格式说明) {类名}可指定简单类名或完整类名；若存在同名类，则需要指定完整类名",
                    "(格式说明) 若存在同名方法，则需要指定方法参数以区分",
                    "(格式说明) {起始代码行号}-{结束代码行号}为可选参数，若不指定则输出指定的整个方法向下的方法完整调用链；若指定则输出方法指定行号范围内向下的方法完整调用链，即 >= 起始代码行号 且 <= 结束代码行号的范围",
                    "(格式说明) {代码行号}可指定某个方法对应的任意代码行号，如C:f1()方法代码起止行号范围为{100,203}，则可指定以上范围的任意数字代表需要处理C:f1()方法",
                    "(格式说明) {方法返回类型}需要指定完整类型",
                    "(示例)",
                    "Test1",
                    "com.test.Test1",
                    "Test1:func1 139-492",
                    "Test1:func1(",
                    "Test1:func1(java.lang.String)",
                    "com.test.Test1:func1 395-1358",
                    "com.test.Test1:func1(",
                    "com.test.Test1:func1(java.lang.String)",
                    "com.test.Test1:func1(java.lang.String):java.lang.String",
                    "Test1:139",
                    "Test1:139 139-492"}
            , null),
    OCFUSE_IGNORE_CALL_TYPE(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_call_type.properties",
            new String[]{"(作用) 生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）",
                    "(内容) 指定忽略的方法调用类型",
                    "(格式) 指定 JavaCG2CallTypeEnum 枚举中的type",
                    "(示例)",
                    "_ITF",
                    "_SCC"}
            , null),
    // todo 配置参数修改为表达式时考虑修改方式
    OCFUSE_IGNORE_METHOD_TYPE_4CALLER(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_method_type_4caller.properties",
            new String[]{"(作用) 生成指定类/方法调用的所有向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）",
                    "(内容) 指定忽略的方法类型",
                    "(格式) 指定 JACGMethodTypeEnum 枚举中的type",
                    "(示例)",
                    "dto.get.set"}
            , null),
    OCFUSE_IGNORE_CLASS_KEYWORD(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_class_keyword.properties",
            new String[]{"(作用) 生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）",
                    "(内容) 指定忽略的类名关键字",
                    "(格式) 可指定包名中的关键字，或类名中的关键字",
                    "(示例)",
                    ".dto.",
                    ".entity.",
                    "Enum",
                    "Constant"}
            , null),
    OCFUSE_IGNORE_FULL_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_full_method_prefix.properties",
            new String[]{"(作用) 生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）",
                    "(内容) 指定忽略的完整方法前缀",
                    "(格式) 可指定包名，或包名+类名，或包名+类名+方法名，或包名+类名+方法名+参数",
                    "(示例)",
                    "com.test",
                    "com.test.Test1",
                    "com.test.Test1:func1",
                    "com.test.Test1:func1(",
                    "com.test.Test1:func1(java.lang.String)"}
            , null),
    OCFUSE_IGNORE_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/ignore_method_prefix.properties",
            new String[]{"(作用) 生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）",
                    "(内容) 指定忽略的方法名前缀",
                    "(示例)",
                    "func1",
                    "func1(",
                    "func1()",
                    "func1(java.lang.String)",
                    "toString()",
                    "hashCode()",
                    "equals(java.lang.Object)",
                    "<init>(",
                    "<clinit>(",
                    "name()",
                    "clone()"}
            , null),
    OCFUSE_INCLUDE_FULL_METHOD_PREFIX(InputDirEnum.IDE_CONFIG.getDirName() + "/include_full_method_prefix.properties",
            new String[]{"(作用) 生成指定类/方法调用的所有向上/向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）",
                    "(内容) 指定需要包含的完整方法前缀",
                    "(优先级低于) allowed_class_prefix.properties、ignore_call_type.properties",
                    "(优先级高于) ignore_class_keyword.properties、ignore_full_method_prefix.properties、ignore_method_prefix.properties",
                    "(格式) 可指定包名，或包名+类名，或包名+类名+方法名，或包名+类名+方法名+参数",
                    "(示例)",
                    "com.test",
                    "com.test.Test1",
                    "com.test.Test1:func1",
                    "com.test.Test1:func1(",
                    "com.test.Test1:func1(java.lang.String)"}
            , null),
    OCFULE_BUSINESS_DATA_TYPE_SHOW_4EE(InputDirEnum.IDE_BUSINESS_DATA_TYPE.getDirName() + "/business_data_type_show_4ee.properties",
            new String[]{"生成向上的完整方法调用链时，需要显示的业务功能数据类型。若不指定则不显示业务功能数据",
                    "默认的业务功能数据类型参考 DefaultBusinessDataTypeEnum 枚举类，supportEe=true的type",
                    DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType(),
                    DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(),
                    DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType()}
            , null),
    OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER(InputDirEnum.IDE_BUSINESS_DATA_TYPE.getDirName() + "/business_data_type_show_4er.properties",
            new String[]{"生成向下的完整方法调用链时，需要显示的业务功能数据类型。若不指定则不显示业务功能数据",
                    "默认的业务功能数据类型参考 DefaultBusinessDataTypeEnum 枚举类，supportEr=true的type",
                    DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType(),
                    DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(),
                    DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType(),
                    DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType(),
                    DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_WRITE_TABLE.getType()}
            , null),
    ;

    // 参数配置文件名
    private final String fileName;
    // 参数配置描述
    private final String[] descriptions;
    // 默认值
    private final String[] defaultValues;

    OtherConfigFileUseSetEnum(String fileName, String[] descriptions, String[] defaultValues) {
        this.fileName = fileName;
        this.descriptions = descriptions;
        this.defaultValues = defaultValues;
    }

    @Override
    public String getEnumConstantsName() {
        return name();
    }

    @Override
    public String getKey() {
        return fileName;
    }

    @Override
    public String[] getDescriptions() {
        return descriptions;
    }

    @Override
    public String[] getDefaultValues() {
        return defaultValues;
    }

    @Override
    public String getConfigPrintInfo() {
        return fileName + " " + OtherConfigFileUseSetEnum.class.getSimpleName() + "." + name();
    }

    @Override
    public boolean isSetOrList() {
        return true;
    }

    @Override
    public OtherConfigFileUseSetEnum getFromKey(String key) {
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
