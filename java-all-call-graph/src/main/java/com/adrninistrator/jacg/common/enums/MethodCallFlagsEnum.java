package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/11/18
 * @description: 方法调用标志枚举
 */
public enum MethodCallFlagsEnum {
    MCFE_ER_METHOD_ANNOTATION(1, "调用方法有注解"),
    MCFE_EE_METHOD_ANNOTATION(1 << 1, "被调用方法有注解"),
    MCFE_METHOD_CALL_INFO(1 << 2, "存在方法调用信息"),
    MCFE_EE_ARGS_WITH_GENERICS_TYPE(1 << 3, "被调用方法参数存在泛型类型"),
    MCFE_ER_ARGS_WITH_GENERICS_TYPE(1 << 4, "调用方法参数存在泛型类型"),
    MCFE_EE_MYBATIS_MAPPER(1 << 5, "被调用方法为Mybatis Mapper"),
    MCFE_EE_MYBATIS_MAPPER_WRITE(1 << 6, "被调用方法为Mybatis写数据库的Mapper方法"),
    MCFE_EE_BUSINESS_DATA(1 << 7, "被调用方法存在业务功能数据"),
    MCFE_EE_RETURN_WITH_GENERICS_TYPE(1 << 8, "被调用方法返回存在泛型类型"),
    MCFE_ER_RETURN_WITH_GENERICS_TYPE(1 << 9, "调用方法返回存在泛型类型"),
    MCFE_EE_DTO_GET_SET_METHOD(1 << 10, "被调用方法属于dto的get/set方法"),
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
