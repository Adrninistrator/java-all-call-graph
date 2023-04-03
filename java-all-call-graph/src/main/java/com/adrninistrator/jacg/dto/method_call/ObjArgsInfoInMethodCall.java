package com.adrninistrator.jacg.dto.method_call;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/12/8
 * @description: 方法调用中被调用对象与参数使用的信息
 */
public class ObjArgsInfoInMethodCall {
    // 被调用对象的信息，列表代表不同的可能
    @JsonProperty("obj")
    private List<MethodCallInfo> objInfo;

    /*
        参数的信息
        key
            参数的序号，从1开始
        value
            参数对应的方法调用中使用的相关信息，列表代表不同的可能，或者数组参数不同的元素
     */
    @JsonProperty("args")
    private Map<Integer, List<MethodCallInfo>> argInfoMap;

    /**
     * 获取指定参数序号对应的参数信息
     *
     * @param argSeq
     * @return
     */
    public List<MethodCallInfo> getArgMethodCallInfo(int argSeq) {
        if (argInfoMap == null) {
            return null;
        }
        return argInfoMap.get(argSeq);
    }

    public List<MethodCallInfo> getObjInfo() {
        return objInfo;
    }

    public void setObjInfo(List<MethodCallInfo> objInfo) {
        this.objInfo = objInfo;
    }

    public Map<Integer, List<MethodCallInfo>> getArgInfoMap() {
        return argInfoMap;
    }

    public void setArgInfoMap(Map<Integer, List<MethodCallInfo>> argInfoMap) {
        this.argInfoMap = argInfoMap;
    }
}
