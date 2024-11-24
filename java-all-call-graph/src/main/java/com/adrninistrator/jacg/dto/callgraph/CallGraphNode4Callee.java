package com.adrninistrator.jacg.dto.callgraph;

import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;

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

    // 当前被调用方法的直接或间接的调用方法的数量
    private final JavaCG2Counter callTimes;

    public CallGraphNode4Callee(String calleeMethodHash, String callerMethodHash, String calleeFullMethod) {
        this.calleeMethodHash = calleeMethodHash;
        this.callerMethodHash = callerMethodHash;
        this.calleeFullMethod = calleeFullMethod;
        callTimes = new JavaCG2Counter();
    }

    @Override
    public String toString() {
        return calleeFullMethod;
    }

    // 增加当前被调用方法的直接或间接的调用方法的数量
    public void addCallTimes() {
        callTimes.addAndGet();
    }

    // 获取当前被调用方法的直接或间接的调用方法的数量
    public int getCallTimes() {
        return callTimes.getCount();
    }

    //
    public String getCalleeMethodHash() {
        return calleeMethodHash;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }
}
