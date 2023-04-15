package com.adrninistrator.jacg.dto.annotation;

/**
 * @author adrninistrator
 * @date 2023/3/9
 * @description: 注解属性值，空值
 */
public class EmptyAnnotationAttribute extends BaseAnnotationAttribute {
    private static final EmptyAnnotationAttribute INSTANCE = new EmptyAnnotationAttribute();

    public static EmptyAnnotationAttribute getInstance() {
        return INSTANCE;
    }

    private EmptyAnnotationAttribute() {
    }
}
