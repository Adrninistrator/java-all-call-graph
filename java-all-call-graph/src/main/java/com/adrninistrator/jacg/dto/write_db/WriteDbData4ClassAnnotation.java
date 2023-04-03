package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 用于写入数据库的数据，类的注解
 */
public class WriteDbData4ClassAnnotation extends AbstractWriteDbData {
    private final String simpleClassName;
    private final String annotationName;
    private final String attributeName;
    private final String annotationType;
    private final String attributeValue;
    private final String className;

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

    public String getAnnotationName() {
        return annotationName;
    }

    public String getAttributeName() {
        return attributeName;
    }

    public String getAnnotationType() {
        return annotationType;
    }

    public String getAttributeValue() {
        return attributeValue;
    }

    public String getClassName() {
        return className;
    }
}
