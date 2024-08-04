package com.adrninistrator.jacg.handler.common.enums;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * @author adrninistrator
 * @date 2024/7/17
 * @description: 类与接口的类型枚举
 */
public enum ClassInterfaceEnum {
    CIE_CLASS("class"),
    CIE_ABSTRACT_CLASS("abstract_class"),
    CIE_INTERFACE("interface"),
    ;

    private final String type;

    ClassInterfaceEnum(String type) {
        this.type = type;
    }

    @JsonValue
    public String getType() {
        return type;
    }

    @Override
    public String toString() {
        return type;
    }
}
