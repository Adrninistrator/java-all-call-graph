package com.adrninistrator.jacg.handler.dto.field;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/10/4
 * @description: 嵌套类型的顶层类型信息
 */
public class NestedFieldTopClassInfo {

    // 嵌套类型的顶层类的类型
    private final String nestedFieldTopClassName;

    // 当前类在顶层类型中的嵌套的字段名称列表
    private final List<String> currentClassNestedFieldNameList;

    // 当前类在顶层类型中的嵌套的字段名称列表，通过@JsonProperty注解支持别名
    private final List<String> currentClassNestedFieldNameJsonAliasList;

    // 以上两个字段名称列表是否存在不同
    private final boolean fieldNameListDifferent;

    public NestedFieldTopClassInfo(String nestedFieldTopClassName, List<String> currentClassNestedFieldNameList, List<String> currentClassNestedFieldNameJsonAliasList,
                                   boolean fieldNameListDifferent) {
        this.nestedFieldTopClassName = nestedFieldTopClassName;
        this.currentClassNestedFieldNameList = currentClassNestedFieldNameList;
        this.currentClassNestedFieldNameJsonAliasList = currentClassNestedFieldNameJsonAliasList;
        this.fieldNameListDifferent = fieldNameListDifferent;
    }

    public String getNestedFieldTopClassName() {
        return nestedFieldTopClassName;
    }

    public List<String> getCurrentClassNestedFieldNameList() {
        return currentClassNestedFieldNameList;
    }

    public List<String> getCurrentClassNestedFieldNameJsonAliasList() {
        return currentClassNestedFieldNameJsonAliasList;
    }

    public boolean isFieldNameListDifferent() {
        return fieldNameListDifferent;
    }
}
