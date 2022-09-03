package com.adrninistrator.jacg.dto.annotation_attribute;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 注解属性值，String格式，对应基本、类、枚举类型的注解属性
 */
public class StringAnnotationAttribute extends BaseAnnotationAttribute {
    private String attributeString;

    public String getAttributeString() {
        return attributeString;
    }

    public void setAttributeString(String attributeString) {
        this.attributeString = attributeString;
    }
}
