package com.adrninistrator.jacg.dto.method;

/**
 * @author adrninistrator
 * @date 2022/11/19
 * @description: 方法调用关系中调用方法与被调用方法完整方法
 */
public class MethodCallFullMethod {
    private final String callerFullMethod;

    private final String calleeFullMethod;

    public MethodCallFullMethod(String callerFullMethod, String calleeFullMethod) {
        this.callerFullMethod = callerFullMethod;
        this.calleeFullMethod = calleeFullMethod;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }
}
