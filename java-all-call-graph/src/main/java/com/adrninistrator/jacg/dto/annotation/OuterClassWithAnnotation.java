package com.adrninistrator.jacg.dto.annotation;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/11/3
 * @description: 外部类类名，及外部类上指定注解的属性
 */
public class OuterClassWithAnnotation {
    // 父类类名
    private final String outerClassName;

    // 外部类上指定注解的属性
    private final Map<String, BaseAnnotationAttribute> annotationAttributeMap;

    public OuterClassWithAnnotation(String outerClassName, Map<String, BaseAnnotationAttribute> annotationAttributeMap) {
        this.outerClassName = outerClassName;
        this.annotationAttributeMap = annotationAttributeMap;
    }

    public String getOuterClassName() {
        return outerClassName;
    }

    public Map<String, BaseAnnotationAttribute> getAnnotationAttributeMap() {
        return annotationAttributeMap;
    }
}
