package com.adrninistrator.jacg.dto.method_call;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/12/8
 * @description: 方法调用中使用的相关信息
 */
public class MethodCallInfo {
    // 可能的类型列表
    @JsonProperty("t")
    private List<String> typeList;

    // 可能的值列表
    @JsonProperty("v")
    private List<String> valueList;

    // 可能的静态字段列表
    @JsonProperty("sf")
    private List<String> staticFieldList;

    // 可能的静态字段的方法列表
    @JsonProperty("sfm")
    private List<String> staticFieldMethodList;

    public List<String> getTypeList() {
        return typeList;
    }

    public void setTypeList(List<String> typeList) {
        this.typeList = typeList;
    }

    public List<String> getValueList() {
        return valueList;
    }

    public void setValueList(List<String> valueList) {
        this.valueList = valueList;
    }

    public List<String> getStaticFieldList() {
        return staticFieldList;
    }

    public void setStaticFieldList(List<String> staticFieldList) {
        this.staticFieldList = staticFieldList;
    }

    public List<String> getStaticFieldMethodList() {
        return staticFieldMethodList;
    }

    public void setStaticFieldMethodList(List<String> staticFieldMethodList) {
        this.staticFieldMethodList = staticFieldMethodList;
    }
}
