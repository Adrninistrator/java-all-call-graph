package com.adrninistrator.jacg.handler.common.enums;

/**
 * @author adrninistrator
 * @date 2023/11/16
 * @description: MyBatis字段与Java代码字段关联方式描述
 */
public enum MyBatisColumnRelateDescEnum {
    MBCRD_ENTITY("e", "使用Entity"),
    MBCRD_OBJECT("o", "使用对象"),
    MBCRD_BASE_TYPE("b", "使用基本类型"),
    ;

    private final String type;

    private final String desc;

    MyBatisColumnRelateDescEnum(String type, String desc) {
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
