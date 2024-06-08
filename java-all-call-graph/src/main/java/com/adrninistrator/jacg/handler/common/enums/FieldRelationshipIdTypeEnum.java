package com.adrninistrator.jacg.handler.common.enums;

/**
 * @author adrninistrator
 * @date 2023/12/8
 * @description: 通过get/set方法的字段关联关系id类型枚举
 */
public enum FieldRelationshipIdTypeEnum {
    FRITE_FIELD_RELATIONSHIP_ID("field_relationship_id", "通过get/set方法的字段关联关系id"),
    FRITE_GET_METHOD_CALL_ID("get_method_call_id", "get方法调用id"),
    FRITE_SET_METHOD_CALL_ID("set_method_call_id", "set方法调用id"),
    ;

    private final String type;
    private final String desc;

    FieldRelationshipIdTypeEnum(String type, String desc) {
        this.type = type;
        this.desc = desc;
    }

    public String getType() {
        return type;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String toString() {
        return type;
    }
}
