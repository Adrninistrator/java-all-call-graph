package com.adrninistrator.jacg.extractor.common.enums;

/**
 * @author adrninistrator
 * @date 2023/2/24
 * @description: Spring事务类型枚举
 */
public enum SpringTxTypeEnum {
    STTE_ANNOTATION("事务注解"),
    STTE_TEMPLATE("事务模板"),
    ;

    SpringTxTypeEnum(String desc) {
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
