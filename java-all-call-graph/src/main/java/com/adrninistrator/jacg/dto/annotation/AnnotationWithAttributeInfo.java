package com.adrninistrator.jacg.dto.annotation;

/**
 * @author adrninistrator
 * @date 2023/4/24
 * @description: 注解及属性信息
 */
public class AnnotationWithAttributeInfo extends AnnotationAttributeInfo {
    private String annotationName;

    public AnnotationWithAttributeInfo() {
    }

    public AnnotationWithAttributeInfo(String attributeName, String attributeType, String attributeValue, String annotationName) {
        super(attributeName, attributeType, attributeValue);
        this.annotationName = annotationName;
    }

    public String getAnnotationName() {
        return annotationName;
    }

    public void setAnnotationName(String annotationName) {
        this.annotationName = annotationName;
    }
}
