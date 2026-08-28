package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2026/6/17
 * @description: SQL语句类型枚举（增删改查）
 */
public enum SqlTypeEnum {
    SELECT("select"),
    INSERT("insert"),
    UPDATE("update"),
    DELETE("delete"),
    OTHER("other");

    private final String type;

    SqlTypeEnum(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }
}
