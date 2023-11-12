package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/11/18
 * @description: 方法调用标记枚举
 */
public enum MethodCallFlagsEnum {
    MCFE_ER_SPRING_CONTROLLER(1, "调用方法属于Spring Controller"),
    MCFE_ER_METHOD_ANNOTATION(1 << 1, "调用方法有注解"),
    MCFE_EE_METHOD_ANNOTATION(1 << 2, "被调用方法有注解"),
    MCFE_METHOD_CALL_INFO(1 << 3, "存在方法调用信息"),
    MCFE_EE_ARGS_WITH_GENERICS_TYPE(1 << 4, "被调用方法参数存在泛型类型"),
    MCFE_ER_ARGS_WITH_GENERICS_TYPE(1 << 5, "调用方法参数存在泛型类型"),
    MCFE_EE_MYBATIS_MAPPER(1 << 6, "被调用方法为Mybatis Mapper"),
    MCFE_EE_MYBATIS_MAPPER_WRITE(1 << 7, "被调用方法为Mybatis写数据库的Mapper方法"),
    MCFE_EE_BUSINESS_DATA(1 << 8, "被调用方法存在自定义的业务功能数据"),
    MCFE_EE_RETURN_WITH_GENERICS_TYPE(1 << 9, "被调用方法返回存在泛型类型"),
    MCFE_ER_RETURN_WITH_GENERICS_TYPE(1 << 10, "调用方法返回存在泛型类型"),
    ;

    // 需要定义为1，以及2的幂
    private final int flag;

    private final String desc;

    MethodCallFlagsEnum(int flag, String desc) {
        this.flag = flag;
        this.desc = desc;
    }

    /**
     * 判断方法调用标志是否包含指定的标志位
     *
     * @param callFlags
     * @return
     */
    public boolean checkFlag(int callFlags) {
        return (callFlags & flag) != 0;
    }

    /**
     * 设置方法调用标志指定的标志位
     *
     * @param callFlags
     * @return
     */
    public int setFlag(int callFlags) {
        return callFlags | flag;
    }

    public int getFlag() {
        return flag;
    }

    public String getDesc() {
        return desc;
    }
}
