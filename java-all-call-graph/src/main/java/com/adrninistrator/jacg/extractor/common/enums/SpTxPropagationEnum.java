package com.adrninistrator.jacg.extractor.common.enums;

import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2023/2/28
 * @description: Spring事务传播行为枚举
 */
public enum SpTxPropagationEnum {
    STPE_DEFAULT_REQUIRED("DEFAULT_REQUIRED"),
    STPE_REQUIRED("REQUIRED"),
    STPE_SUPPORTS("SUPPORTS"),
    STPE_MANDATORY("MANDATORY"),
    STPE_REQUIRES_NEW("REQUIRES_NEW"),
    STPE_NOT_SUPPORTED("NOT_SUPPORTED"),
    STPE_NEVER("NEVER"),
    STPE_NESTED("NESTED"),
    STPE_ILLEGAL("-"),
    ;

    SpTxPropagationEnum(String propagation) {
        this.propagation = propagation;
    }

    private final String propagation;

    public String getPropagation() {
        return propagation;
    }

    /**
     * 根据事务行为字符串获取对应的事务行为
     *
     * @param propagation
     * @return
     */
    public static SpTxPropagationEnum getFromPropagation(String propagation) {
        if (StringUtils.equalsAny(propagation, STPE_DEFAULT_REQUIRED.propagation, STPE_REQUIRED.propagation)) {
            return STPE_REQUIRED;
        }

        for (SpTxPropagationEnum spTxPropagationEnum : SpTxPropagationEnum.values()) {
            if (spTxPropagationEnum.propagation.equals(propagation)) {
                return spTxPropagationEnum;
            }
        }

        return STPE_ILLEGAL;
    }
}
