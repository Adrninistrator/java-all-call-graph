package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2026/6/27
 * @description: 数据库索引类型枚举
 */
public enum IndexTypeEnum {
    PRIMARY("primary"),
    UNIQUE("unique"),
    NORMAL("normal");

    private final String type;

    IndexTypeEnum(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }
}
