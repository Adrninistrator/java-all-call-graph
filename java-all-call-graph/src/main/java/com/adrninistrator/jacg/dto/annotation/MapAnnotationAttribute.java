package com.adrninistrator.jacg.dto.annotation;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 注解属性值，Map格式，对应注解类型的注解属性
 */
public class MapAnnotationAttribute extends BaseAnnotationAttribute {
    private final Map<String, Object> attributeMap;

    public MapAnnotationAttribute(Map<String, Object> attributeMap) {
        this.attributeMap = attributeMap;
    }

    public Map<String, Object> getAttributeMap() {
        return attributeMap;
    }
}
