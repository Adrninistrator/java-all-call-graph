package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2024/7/5
 * @description: 方法类型枚举
 */
public enum JACGMethodTypeEnum {

    MTE_DTO_GET_SET("dto.get.set", "dto的get/set方法"),
    ;

    private final String type;

    private final String desc;

    JACGMethodTypeEnum(String type, String desc) {
        this.type = type;
        this.desc = desc;
    }

    public String getType() {
        return type;
    }

    public String getDesc() {
        return desc;
    }
}
