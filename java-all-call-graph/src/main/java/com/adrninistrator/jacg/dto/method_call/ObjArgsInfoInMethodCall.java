package com.adrninistrator.jacg.dto.method_call;

import com.adrninistrator.jacg.util.JACGUtil;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.ArrayList;
import java.util.Collections;
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
     * 判断参数的信息是否为空
     *
     * @return
     */
    @JsonIgnore
    public boolean isArgInfoMapEmpty() {
        return JACGUtil.isMapEmpty(argInfoMap);
    }

    /**
     * 获取参数的序号列表，已排序
     * 序号从1开始
     *
     * @return
     */
    @JsonIgnore
    public List<Integer> getArgSeqList() {
        if (isArgInfoMapEmpty()) {
            return Collections.emptyList();
        }
        List<Integer> argSeqList = new ArrayList<>(argInfoMap.keySet());
        Collections.sort(argSeqList);
        return argSeqList;
    }

    /**
     * 获取指定参数序号对应的参数信息
     *
     * @param argSeq
     * @return
     */
    @JsonIgnore
    public List<MethodCallInfo> getArgMethodCallInfo(Integer argSeq) {
        if (isArgInfoMapEmpty()) {
            return Collections.emptyList();
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
