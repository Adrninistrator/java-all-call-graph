package com.adrninistrator.jacg.dto.methodcall;

/**
 * @author adrninistrator
 * @date 2026/7/4
 * @description: 方法被调用/调用次数统计（热点方法），用于 MethodCallHandler.queryTopMethods 返回结果
 */
public class MethodCallCountInfo {
    /**
     * 完整方法（callee_full_method 或 caller_full_method，取决于 direction）
     */
    private String fullMethod;

    /**
     * 方法 HASH+长度（callee_method_hash 或 caller_method_hash，取决于 direction）
     */
    private String methodHash;

    /**
     * 调用次数
     */
    private long callCount;

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public long getCallCount() {
        return callCount;
    }

    public void setCallCount(long callCount) {
        this.callCount = callCount;
    }
}
