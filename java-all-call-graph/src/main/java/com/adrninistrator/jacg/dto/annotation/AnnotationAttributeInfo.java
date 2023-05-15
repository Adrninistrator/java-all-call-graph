package com.adrninistrator.jacg.dto.annotation;

/**
 * @author adrninistrator
 * @date 2023/4/24
 * @description: 注解属性信息
 */
public class AnnotationAttributeInfo {
    private String attributeName;
    private String attributeType;
    private String attributeValue;

    public AnnotationAttributeInfo() {
    }

    public AnnotationAttributeInfo(String attributeName, String attributeType, String attributeValue) {
        this.attributeName = attributeName;
        this.attributeType = attributeType;
        this.attributeValue = attributeValue;
    }

    public String getAttributeName() {
        return attributeName;
    }

    public void setAttributeName(String attributeName) {
        this.attributeName = attributeName;
    }

    public String getAttributeType() {
        return attributeType;
    }

    public void setAttributeType(String attributeType) {
        this.attributeType = attributeType;
    }

    public String getAttributeValue() {
        return attributeValue;
    }

    public void setAttributeValue(String attributeValue) {
        this.attributeValue = attributeValue;
    }
}
