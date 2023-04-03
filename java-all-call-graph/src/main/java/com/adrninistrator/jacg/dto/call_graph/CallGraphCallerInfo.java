package com.adrninistrator.jacg.dto.call_graph;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description: 调用方信息
 */
public class CallGraphCallerInfo {
    private final Integer callId;
    private final String callerFullMethod;
    private final Integer callFlags;

    public CallGraphCallerInfo(Integer callId, String callerFullMethod, Integer callFlags) {
        this.callId = callId;
        this.callerFullMethod = callerFullMethod;
        this.callFlags = callFlags;
    }

    public Integer getCallId() {
        return callId;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public Integer getCallFlags() {
        return callFlags;
    }
}
