package com.adrninistrator.jacg.conf.enums;

import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.JavaCG2ConfigKeyEnum;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public enum OtherConfigFileUseSetEnum implements OtherConfigInterface {
    OCFUSE_METHOD_CLASS_4CALLEE(InputDirEnum.IDE_GEN_ALL_CALL_GRAPH.getDirName() + "/method_class_4callee.properties",
            new String[]{"(作用) 生成调用指定类或方法向上的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）",
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
    OCFUSE_METHOD_CLASS_4CALLER(InputDirEnum.IDE_GEN_ALL_CALL_GRAPH.getDirName() + "/method_class_4caller.properties",
            new String[]{"(作用) 生成指定类调用或方法向下的方法完整调用链时的配置文件（每行指定一项配置，可指定多行）",
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
    OCFUSE_CALLER_GRAPH_CALLEE_ARG_TYPE_POLYMORPHISM(InputDirEnum.IDE_GEN_ALL_CALL_GRAPH.getDirName() + "/caller_graph_callee_arg_type_polymorphism.properties",
            new String[]{"(作用) 生成向下方法完整调用链时，指定哪些方法参数作为被调用对象涉及多态时的类型替换（每行指定一项配置，可指定多行）",
                    "(作用) 即对被调用类型使用实际传入的子类类型替换方法参数定义的父类类型",
                    "(前提) 使用 java-callgraph2 组件解析方法调用时需要将 " + JavaCG2ConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE.getKey() + " 参数值设置为 " + Boolean.TRUE,
                    "(限制) 仅支持获取被调用方法被直接调用（没有嵌套多层调用）时的子类类型",
                    "(限制) 仅支持调用被调用方法时使用一种子类类型，不支持多种",
                    "(格式1) {参数作为被调用对象时需要替换被调用类型的完整方法}={对应的参数序号，从1开始}",
                    "(格式2) {参数作为被调用对象时需要替换被调用类型的完整方法}:{方法返回类型}={对应的参数序号，从1开始}",
                    "(示例)",
                    "a.b.C:f1(int,a.b.Super)=2",
                    "a.b.C:f1(int,a.b.Super):java.lang.String=1"}
            , null),
    OCFULE_BUSINESS_DATA_TYPE_SHOW_4EE(InputDirEnum.IDE_BUSINESS_DATA_TYPE.getDirName() + "/business_data_type_show_4ee.properties",
            new String[]{"生成向上的方法完整调用链时，指定默认支持的业务功能数据需要显示哪些类型。若不指定则不显示",
                    "默认支持的业务功能数据类型参考 " + DefaultBusinessDataTypeEnum.class.getSimpleName() + " 枚举类，supportEe=true的type",
                    DefaultBusinessDataTypeEnum.getSupportTypeStr(true)}
            , null),
    OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER(InputDirEnum.IDE_BUSINESS_DATA_TYPE.getDirName() + "/business_data_type_show_4er.properties",
            new String[]{"生成向下的方法完整调用链时，指定默认支持的业务功能数据需要显示哪些类型。若不指定则不显示",
                    "默认支持的业务功能数据类型参考 " + DefaultBusinessDataTypeEnum.class.getSimpleName() + " 枚举类，supportEr=true的type",
                    DefaultBusinessDataTypeEnum.getSupportTypeStr(false)}
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
    public String getEnumConstantName() {
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
    public String genConfigUsage() {
        return doGenConfigUsage(ConfigureWrapper.class);
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
