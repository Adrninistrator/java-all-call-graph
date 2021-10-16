package com.adrninistrator.jacg.extension.enums;

/**
 * @author adrninistrator
 * @date 2021/9/9
 * @description:
 */
public enum DbStatementEnum {

    DSE_SELECT("select"),
    DSE_INSERT("insert"),
    DSE_REPLACE("replace"),
    DSE_UPDATE("update"),
    DSE_DELETE("delete");

    private String value;

    DbStatementEnum(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
