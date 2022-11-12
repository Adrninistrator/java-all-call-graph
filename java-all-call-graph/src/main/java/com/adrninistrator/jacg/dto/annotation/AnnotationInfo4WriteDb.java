package com.adrninistrator.jacg.dto.annotation;

/**
 * @author adrninistrator
 * @date 2022/4/6
 * @description:
 */
public class AnnotationInfo4WriteDb {
    private final String classOrMethodName;

    private final String annotationName;

    private final String attributeName;

    private final String attributeValue;

    public AnnotationInfo4WriteDb(String classOrMethodName, String annotationName, String attributeName, String attributeValue) {
        this.classOrMethodName = classOrMethodName;
        this.annotationName = annotationName;
        this.attributeName = attributeName;
        this.attributeValue = attributeValue;
    }

    public String getClassOrMethodName() {
        return classOrMethodName;
    }

    public String getAnnotationName() {
        return annotationName;
    }

    public String getAttributeName() {
        return attributeName;
    }

    public String getAttributeValue() {
        return attributeValue;
    }
}
