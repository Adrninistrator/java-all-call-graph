package com.adrninistrator.jacg.handler.common.enums;

/**
 * @author adrninistrator
 * @date 2023/12/5
 * @description: set方法赋值情况标志枚举
 */
public enum SetMethodAssignFlagEnum {
    SMAFE_GET("get", "set方法都使用了get方法返回值作为参数"),
    SMAFE_CONSTANT("constant", "set方法赋值时使用常量"),
    SMAFE_NON_STATIC_FIELD("non_static_field", "set方法赋值时使用非静态字段"),
    SMAFE_STATIC_FIELD("static_field", "set方法赋值时使用静态字段"),
    SMAFE_MC_NO_RETURN("mc_no_return", "set方法赋值时使用的方法调用返回的调用信息未找到"),
    SMAFE_MC_ENUM("mc_enum", "set方法赋值时使用的方法调用被调用类为枚举"),
    SMAFE_ARG_NO_MC("arg_no_mc", "set方法赋值时使用方法参数，该方法未被调用"),
    SMAFE_ARG_NO_MCI("arg_no_mci", "set方法赋值时使用方法参数，未找到方法被调用时的参数信息"),
    SMAFE_GET_BY_MCP("get_by_mcp", "set方法赋值的来源为通过方法调用传递的get方法"),
    SMAFE_METHOD_CALL_RETURN("mcr", "set方法赋值的来源通过方法调用返回值进行传递"),
    SMAFE_METHOD_CALL_ARGS("mca", "set方法赋值的来源通过方法调用参数进行传递"),
    ;

    private final String flag;
    private final String desc;

    SetMethodAssignFlagEnum(String flag, String desc) {
        this.flag = flag;
        this.desc = desc;
    }

    public String getFlag() {
        return flag;
    }

    public String getDesc() {
        return desc;
    }
}
