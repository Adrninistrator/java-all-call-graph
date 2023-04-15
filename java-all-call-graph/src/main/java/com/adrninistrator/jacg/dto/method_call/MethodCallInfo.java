package com.adrninistrator.jacg.dto.method_call;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author adrninistrator
 * @date 2022/12/8
 * @description: 方法调用中使用的相关信息
 */
public class MethodCallInfo {
    // 可能的类型列表
    @JsonProperty("t")
    private String type;

    // 可能的值列表
    @JsonProperty("v")
    private String value;

    // 可能的静态字段列表
    @JsonProperty("sf")
    private String staticField;

    // 可能的静态字段的方法列表
    @JsonProperty("sfm")
    private String staticFieldMethod;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getStaticField() {
        return staticField;
    }

    public void setStaticField(String staticField) {
        this.staticField = staticField;
    }

    public String getStaticFieldMethod() {
        return staticFieldMethod;
    }

    public void setStaticFieldMethod(String staticFieldMethod) {
        this.staticFieldMethod = staticFieldMethod;
    }

    @Override
    public String toString() {
        return "MethodCallInfo{" +
                "type='" + type + '\'' +
                ", value='" + value + '\'' +
                ", staticField='" + staticField + '\'' +
                ", staticFieldMethod='" + staticFieldMethod + '\'' +
                '}';
    }
}
