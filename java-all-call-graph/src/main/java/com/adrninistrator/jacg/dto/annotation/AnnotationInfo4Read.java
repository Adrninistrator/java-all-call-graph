package com.adrninistrator.jacg.dto.annotation;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/4/6
 * @description:
 */
public class AnnotationInfo4Read {
    // 注解类名
    private String annotationName;

    /*
        注解属性Map
        key 属性名称
        value 属性值
     */
    private Map<String, String> annotationAttributeMap;

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
