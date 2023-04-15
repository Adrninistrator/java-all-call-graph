package com.adrninistrator.jacg.dto.annotation;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 注解属性值，List格式，对应数组类型的注解属性，内部属性类型为Map
 */
public class ListMapAnnotationAttribute extends BaseAnnotationAttribute {
    private final List<Map<String, Object>> attributeList;

    public ListMapAnnotationAttribute(List<Map<String, Object>> attributeList) {
        this.attributeList = attributeList;
    }

    public List<Map<String, Object>> getAttributeList() {
        return attributeList;
    }
}
