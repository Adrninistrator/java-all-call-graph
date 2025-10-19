package com.adrninistrator.jacg.el.enums;

import com.adrninistrator.jacg.common.enums.MethodCallFlagsEnum;
import com.adrninistrator.javacg2.el.enums.interfaces.ElAllowedVariableInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/2/15
 * @description: 允许使用表达式语言的变量枚举
 */
public enum ElAllowedVariableEnum implements ElAllowedVariableInterface {

    EAVE_MC_FLAGS_ENUM("flags_enum", List.class.getSimpleName() + "<" + String.class.getSimpleName() + ">",
            new String[]{"方法调用标志枚举",
                    "指定 " + MethodCallFlagsEnum.class.getSimpleName() + " 类的常量名称，代表需要处理包含/不包含哪些标志的方法调用",
                    "如 " + MethodCallFlagsEnum.values()[0].name()},
            MethodCallFlagsEnum.genShowInfo()),
    ;

    // 变量名称
    private final String name;

    // 变量类型
    private final String type;

    // 变量说明
    private final String[] descriptions;

    // 变量值示例
    private final String[] valueExamples;

    ElAllowedVariableEnum(String name, String type, String[] descriptions, String[] valueExamples) {
        this.name = name;
        this.type = type;
        this.descriptions = descriptions;
        this.valueExamples = valueExamples;
    }

    @Override
    public String getVariableName() {
        return name;
    }

    @Override
    public String getType() {
        return type;
    }

    @Override
    public boolean isPrefixWithNum() {
        return false;
    }

    @Override
    public String[] getDescriptions() {
        return descriptions;
    }

    @Override
    public String[] getValueExamples() {
        return valueExamples;
    }

    @Override
    public String getEnumConstantName() {
        return name();
    }

    @Override
    public String toString(){
        throw new JavaCG2RuntimeException("为避免误用，当前方法不允许调用，应当使用 getVariableName 方法");
    }
}
