package com.adrninistrator.jacg.dto.methodcall;

/**
 * @author adrninistrator
 * @date 2023/2/23
 * @description: 包含调用方与被调用方的方法调用信息
 */
public class MethodCallPair {
    // 调用方方法名称+类型
    protected final String callerMethodAndArgs;

    // 调用方方法返回类型
    protected final String callerMethodReturnType;

    // 被调用方完整方法
    protected final String calleeMethodAndArgs;

    // 被调用方方法返回类型
    protected final String calleeMethodReturnType;

    public MethodCallPair(String callerMethodAndArgs, String callerMethodReturnType, String calleeMethodAndArgs, String calleeMethodReturnType) {
        this.callerMethodAndArgs = callerMethodAndArgs;
        this.callerMethodReturnType = callerMethodReturnType;
        this.calleeMethodAndArgs = calleeMethodAndArgs;
        this.calleeMethodReturnType = calleeMethodReturnType;
    }

    public String getCallerMethodAndArgs() {
        return callerMethodAndArgs;
    }

    public String getCallerMethodReturnType() {
        return callerMethodReturnType;
    }

    public String getCalleeMethodAndArgs() {
        return calleeMethodAndArgs;
    }

    public String getCalleeMethodReturnType() {
        return calleeMethodReturnType;
    }
}
