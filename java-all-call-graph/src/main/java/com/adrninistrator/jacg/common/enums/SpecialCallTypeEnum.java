package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2023/2/26
 * @description: 特殊的方法调用类型
 */
public enum SpecialCallTypeEnum {
    SCTE_LAMBDA("Lambda表达式", 1),
    SCTE_ANONYMOUS_INNER_CLASS("匿名内部类", 2),
    ;

    SpecialCallTypeEnum(String desc, int order) {
        this.desc = desc;
        this.order = order;
    }

    private final String desc;

    private final int order;

    public String getDesc() {
        return desc;
    }

    public int getOrder() {
        return order;
    }

    @Override
    public String toString() {
        return name();
    }
}
