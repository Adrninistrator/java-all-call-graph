package com.adrninistrator.jacg.dto.annotation;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/4/8
 * @description: 父类类名，及父类上指定注解的属性
 */
public class SuperClassWithAnnotation {
    // 父类类名
    private final String superClassName;

    // 父类上指定注解的属性
    private final Map<String, BaseAnnotationAttribute> annotationAttributeMap;

    public SuperClassWithAnnotation(String superClassName, Map<String, BaseAnnotationAttribute> annotationAttributeMap) {
        this.superClassName = superClassName;
        this.annotationAttributeMap = annotationAttributeMap;
    }

    public String getSuperClassName() {
        return superClassName;
    }

    public Map<String, BaseAnnotationAttribute> getAnnotationAttributeMap() {
        return annotationAttributeMap;
    }
}
