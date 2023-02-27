package com.adrninistrator.jacg.dto.method_call;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/12/8
 * @description: 方法调用中被调用对象与参数使用的信息
 */
public class ObjArgsInfoInMethodCall {
    // 被调用对象的信息
    @JsonProperty("obj")
    private MethodCallInfo objInfo;

    /*
        参数的信息
        key
            参数的序号，从0开始
        value
            参数对应的方法调用中使用的相关信息
     */
    @JsonProperty("args")
    private Map<Integer, MethodCallInfo> argInfoMap;

    public MethodCallInfo getObjInfo() {
        return objInfo;
    }

    public void setObjInfo(MethodCallInfo objInfo) {
        this.objInfo = objInfo;
    }

    public Map<Integer, MethodCallInfo> getArgInfoMap() {
        return argInfoMap;
    }

    public void setArgInfoMap(Map<Integer, MethodCallInfo> argInfoMap) {
        this.argInfoMap = argInfoMap;
    }
}
