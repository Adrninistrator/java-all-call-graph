package com.adrninistrator.jacg.dto.annotation;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/4/6
 * @description:
 */
public class AnnotationInfo4Write {
    private String classOrMethodName;

    private String annotationName;

    private Map<String, String> annotationAttributeMap;

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

    public Map<String, String> getAnnotationAttributeMap() {
        return annotationAttributeMap;
    }

    public void setAnnotationAttributeMap(Map<String, String> annotationAttributeMap) {
        this.annotationAttributeMap = annotationAttributeMap;
    }
}
