package com.adrninistrator.jacg.dto.annotation_attribute;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 注解属性值，Map格式，对应注解类型的注解属性
 */
public class MapAnnotationAttribute extends BaseAnnotationAttribute {
    private Map<String, Object> attributeMap;

    public Map<String, Object> getAttributeMap() {
        return attributeMap;
    }

    public void setAttributeMap(Map<String, Object> attributeMap) {
        this.attributeMap = attributeMap;
    }
}
