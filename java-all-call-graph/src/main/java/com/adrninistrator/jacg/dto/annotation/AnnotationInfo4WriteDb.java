package com.adrninistrator.jacg.dto.annotation;

/**
 * @author adrninistrator
 * @date 2022/4/6
 * @description:
 */
public class AnnotationInfo4WriteDb {
    private String classOrMethodName;

    private String annotationName;

    private String attributeName;

    private String attributeValue;

    public String getClassOrMethodName() {
        return classOrMethodName;
    }

    public void setClassOrMethodName(String classOrMethodName) {
        this.classOrMethodName = classOrMethodName;
    }

    public String getAnnotationName() {
        return annotationName;
    }

    public void setAnnotationName(String annotationName) {
        this.annotationName = annotationName;
    }

    public String getAttributeName() {
        return attributeName;
    }

    public void setAttributeName(String attributeName) {
        this.attributeName = attributeName;
    }

    public String getAttributeValue() {
        return attributeValue;
    }

    public void setAttributeValue(String attributeValue) {
        this.attributeValue = attributeValue;
    }
}
