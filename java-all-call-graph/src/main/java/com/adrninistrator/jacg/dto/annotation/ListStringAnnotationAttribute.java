package com.adrninistrator.jacg.dto.annotation;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description: 注解属性值，List格式，对应数组类型的注解属性，内部属性类型为String
 */
public class ListStringAnnotationAttribute extends BaseAnnotationAttribute {
    private final List<String> attributeList;

    public ListStringAnnotationAttribute(List<String> attributeList) {
        this.attributeList = attributeList;
    }

    public List<String> getAttributeList() {
        return attributeList;
    }
}
