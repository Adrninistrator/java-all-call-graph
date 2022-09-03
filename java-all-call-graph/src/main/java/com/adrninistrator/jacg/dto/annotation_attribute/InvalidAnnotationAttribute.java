package com.adrninistrator.jacg.dto.annotation_attribute;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description:
 */
public class InvalidAnnotationAttribute extends BaseAnnotationAttribute {
    private static final InvalidAnnotationAttribute instance = new InvalidAnnotationAttribute();

    public static InvalidAnnotationAttribute getInstance() {
        return instance;
    }

    private InvalidAnnotationAttribute() {
    }
}
