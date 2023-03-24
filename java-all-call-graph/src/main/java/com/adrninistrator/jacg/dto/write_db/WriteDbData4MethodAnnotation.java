package com.adrninistrator.jacg.dto.write_db;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 用于写入数据库的数据，方法的注解
 */
public class WriteDbData4MethodAnnotation extends AbstractWriteDbData {
    private String methodHash;

    private String annotationName;

    private String attributeName;

    private String annotationType;

    private String attributeValue;

    private String fullMethod;

    private String simpleClassName;

    private int springMappingAnnotation;

    //
    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
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

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public int getSpringMappingAnnotation() {
        return springMappingAnnotation;
    }

    public void setSpringMappingAnnotation(int springMappingAnnotation) {
        this.springMappingAnnotation = springMappingAnnotation;
    }
}
