package com.adrninistrator.jacg.dto.annotation;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 注解属性值，非法值
 */
public class InvalidAnnotationAttribute extends BaseAnnotationAttribute {
    private static final InvalidAnnotationAttribute INSTANCE = new InvalidAnnotationAttribute();

    public static InvalidAnnotationAttribute getInstance() {
        return INSTANCE;
    }

    private InvalidAnnotationAttribute() {
    }
}
