package com.adrninistrator.jacg.dto.annotation;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 注解属性值，String格式，对应基本、类、枚举类型的注解属性
 */
public class StringAnnotationAttribute extends BaseAnnotationAttribute {
    private final String attributeString;

    public StringAnnotationAttribute(String attributeString) {
        this.attributeString = attributeString;
    }

    public String getAttributeString() {
        return attributeString;
    }
}
