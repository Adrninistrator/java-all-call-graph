package com.adrninistrator.jacg.handler.dto.generics_type;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/23
 * @description: 方法参数泛型类型值
 */
public class GenericsTypeValue {
    @JsonProperty("t")
    private String type;

    @JsonProperty("gt")
    private final List<String> genericsTypeList = new ArrayList<>();

    public void addGenericsType(String genericsType) {
        genericsTypeList.add(genericsType);
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public List<String> getGenericsTypeList() {
        return genericsTypeList;
    }
}
