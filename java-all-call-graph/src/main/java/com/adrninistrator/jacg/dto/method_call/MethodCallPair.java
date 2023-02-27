package com.adrninistrator.jacg.dto.method_call;

/**
 * @author adrninistrator
 * @date 2023/2/23
 * @description: 包含调用方与被调用方的方法调用信息
 */
public class MethodCallPair {
    // 调用方完整方法
    private final String callerFullMethod;

    // 调用方代码行号
    private final int callerLineNumber;

    // 被调用方完整方法
    private final String calleeFullMethod;

    public MethodCallPair(String callerFullMethod, int callerLineNumber, String calleeFullMethod) {
        this.callerFullMethod = callerFullMethod;
        this.callerLineNumber = callerLineNumber;
        this.calleeFullMethod = calleeFullMethod;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public int getCallerLineNumber() {
        return callerLineNumber;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }
}
