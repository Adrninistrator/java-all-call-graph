package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2023/3/7
 * @description:
 */
public enum YesNoEnum {
    YES("是"),
    NO("否"),
    ;

    private final String desc;

    YesNoEnum(String desc) {
        this.desc = desc;
    }

    public static String parse(boolean value) {
        return value ? YES.getDesc() : NO.getDesc();
    }

    public String getDesc() {
        return desc;
    }
}
