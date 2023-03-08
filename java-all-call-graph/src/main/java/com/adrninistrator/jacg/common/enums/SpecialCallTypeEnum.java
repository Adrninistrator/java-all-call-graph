package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2023/2/26
 * @description: 特殊的方法调用类型
 */
public enum SpecialCallTypeEnum {
    SCTE_LAMBDA("Lambda表达式"),
    SCTE_ANONYMOUS_INNER_CLASS("匿名内部类"),
    ;

    SpecialCallTypeEnum(String desc) {
        this.desc = desc;
    }

    private final String desc;

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return name();
    }
}
