package com.adrninistrator.jacg.dto.callgraph;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 生成调用指定类的所有向上的调用关系时，使用的栈中的节点
 */

public class CallGraphNode4Callee {
    // 当前被调用方法HASH+长度
    private final String calleeMethodHash;

    // 当前调用方法HASH+长度
    private String callerMethodHash;

    // 当前被调用方法的完整方法
    private final String calleeFullMethod;

    public CallGraphNode4Callee(String calleeMethodHash, String callerMethodHash, String calleeFullMethod) {
        this.calleeMethodHash = calleeMethodHash;
        this.callerMethodHash = callerMethodHash;
        this.calleeFullMethod = calleeFullMethod;
    }

    @Override
    public String toString() {
        return calleeFullMethod;
    }

    // get
    public String getCalleeMethodHash() {
        return calleeMethodHash;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    // set
    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }
}
