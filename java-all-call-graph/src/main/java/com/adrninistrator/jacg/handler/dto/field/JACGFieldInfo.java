package com.adrninistrator.jacg.handler.dto.field;

/**
 * @author adrninistrator
 * @date 2023/8/9
 * @description: 字段信息
 */
public class JACGFieldInfo {

    // 字段名称
    private final String fieldName;

    // 字段类型
    private final String fieldType;

    // 字段完整层级的显示名称，使用.代表嵌套类型中的字段，支持@JsonProperty注解中的名称
    private final String fieldCompleteShowName;

    // 字段的@JsonProperty注解属性值
    private final String fieldJsonPropertyValue;

    // 字段所在的类名，可能是在指定的类中，或者在嵌套类型中
    private final String className;

    public JACGFieldInfo(String fieldName, String fieldType, String fieldCompleteShowName, String fieldJsonPropertyValue, String className) {
        this.fieldName = fieldName;
        this.fieldType = fieldType;
        this.fieldCompleteShowName = fieldCompleteShowName;
        this.fieldJsonPropertyValue = fieldJsonPropertyValue;
        this.className = className;
    }

    public String getFieldName() {
        return fieldName;
    }

    public String getFieldType() {
        return fieldType;
    }

    public String getFieldCompleteShowName() {
        return fieldCompleteShowName;
    }

    public String getFieldJsonPropertyValue() {
        return fieldJsonPropertyValue;
    }

    public String getClassName() {
        return className;
    }
}
