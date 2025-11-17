package com.adrninistrator.jacg.dto.methodcall;

import com.fasterxml.jackson.annotation.JsonIgnore;

/**
 * @author adrninistrator
 * @date 2025/2/24
 * @description: 方法完整调用链当前行的数据，对应生成向上的完整调用链
 */
public class MethodCallLineData4Er extends MethodCallLineData {

    // 被调用方法信息
    @JsonIgnore
    private String calleeInfo;

    // 当前被调用方法向下是否没有被调用方法
    private boolean noDownwardCallee;

    public MethodCallLineData4Er(int methodCallLevel, String callerSimpleClassName, Integer callerLineNumber, String actualFullMethod, String rawMethodHash,
                                 String actualMethodHash, String methodReturnType, int methodCallId, Integer callFlags, String callType) {
        super(methodCallLevel, callerSimpleClassName, callerLineNumber, actualFullMethod, rawMethodHash, actualMethodHash, methodReturnType, methodCallId, callFlags, callType);
    }

    public String getCalleeInfo() {
        return calleeInfo;
    }

    public void setCalleeInfo(String calleeInfo) {
        this.calleeInfo = calleeInfo;
    }

    public boolean isNoDownwardCallee() {
        return noDownwardCallee;
    }

    public void setNoDownwardCallee(boolean noDownwardCallee) {
        this.noDownwardCallee = noDownwardCallee;
    }
}
