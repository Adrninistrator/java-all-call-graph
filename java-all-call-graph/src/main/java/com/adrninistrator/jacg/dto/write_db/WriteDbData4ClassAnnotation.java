package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 用于写入数据库的数据，类的注解
 */
public class WriteDbData4ClassAnnotation extends AbstractWriteDbData {
    private String simpleClassName;
    private String annotationName;
    private String attributeName;
    private String annotationType;
    private String attributeValue;
    private String className;

    public WriteDbData4ClassAnnotation() {
    }

    public WriteDbData4ClassAnnotation(String simpleClassName, String annotationName, String attributeName, String annotationType, String attributeValue, String className) {
        this.simpleClassName = simpleClassName;
        this.annotationName = annotationName;
        this.attributeName = attributeName;
        this.annotationType = annotationType;
        this.attributeValue = attributeValue;
        this.className = className;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
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

    public String getAnnotationType() {
        return annotationType;
    }

    public void setAnnotationType(String annotationType) {
        this.annotationType = annotationType;
    }

    public String getAttributeValue() {
        return attributeValue;
    }

    public void setAttributeValue(String attributeValue) {
        this.attributeValue = attributeValue;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }
}
