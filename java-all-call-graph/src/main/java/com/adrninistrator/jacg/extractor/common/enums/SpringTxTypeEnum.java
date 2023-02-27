package com.adrninistrator.jacg.extractor.common.enums;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务类型枚举
 */
public enum SpringTxTypeEnum {
    STTE_ANNOTATION("事务注解", 1),
    STTE_TEMPLATE("事务模板", 2),
    ;

    SpringTxTypeEnum(String desc, int order) {
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
