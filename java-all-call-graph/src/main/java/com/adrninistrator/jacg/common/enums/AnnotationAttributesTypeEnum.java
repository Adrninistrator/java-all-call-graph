package com.adrninistrator.jacg.common.enums;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description 注解属性类型
 */
public enum AnnotationAttributesTypeEnum {
    // 不支持的类型
    AATE_NOT_SUPPORT("not_support", ""),
    // 字符串类型，对应简单类型、类、枚举类型，属性值为字符串
    AATE_STRING("string", "s"),
    // 字符串类型，对应简单类型、类、枚举类型，进行BASE64编码，属性值为字符串BASE64编码后的结果
    AATE_STRING_BASE64("string_base64", "bs"),
    // Map类型，对应注解类型，属性值为JSON字符串
    AATE_MAP("map", "m"),
    // List类型，对应数组类型，内部属性类型为String，属性值为JSON字符串
    AATE_LIST_STRING("list_string", "ls"),
    // List类型，对应数组类型，内部属性类型为Map，属性值为JSON字符串
    AATE_LIST_MAP("list_map", "lm"),
    ;

    private final String type;

    private final String prefix;

    AnnotationAttributesTypeEnum(String type, String prefix) {
        this.type = type;
        this.prefix = prefix;
    }

    public String getType() {
        return type;
    }

    public String getPrefix() {
        return prefix;
    }

    public int getPrefixLength() {
        return prefix.length();
    }
}
