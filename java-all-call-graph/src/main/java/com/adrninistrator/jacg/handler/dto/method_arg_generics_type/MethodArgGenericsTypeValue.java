package com.adrninistrator.jacg.handler.dto.method_arg_generics_type;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/23
 * @description: 方法参数泛型类型值
 */
public class MethodArgGenericsTypeValue {
    @JsonProperty("t")
    private String argType;

    @JsonProperty("gt")
    private final List<String> argGenericsTypeList = new ArrayList<>();

    public void addArgGenericsType(String argGenericsType) {
        argGenericsTypeList.add(argGenericsType);
    }

    public String getArgType() {
        return argType;
    }

    public void setArgType(String argType) {
        this.argType = argType;
    }

    public List<String> getArgGenericsTypeList() {
        return argGenericsTypeList;
    }
}
